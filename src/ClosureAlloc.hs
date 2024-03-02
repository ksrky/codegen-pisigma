{-# LANGUAGE TemplateHaskell #-}

module ClosureNameless (closureNamelessProgram) where

import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Foldable
import Data.List                qualified as List
import Id
import Nameless                 qualified as N
import Prelude                  hiding (exp)

data Ctx = Ctx {
    _varScope   :: [Id],
    _funScope   :: [(Id, N.Global)],
    _labelIndex :: [(String, Int)]}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

runCtxM :: CtxM a -> IO a
runCtxM m = runReaderT m (Ctx [] [] [])

closureNamelessTy :: C.Ty -> CtxM N.Ty
closureNamelessTy = cata $ \case
    C.TIntF        -> return N.TInt
    C.TVarF x      -> do
        vsc <- view varScope
        case List.elemIndex x vsc of
            Just i  -> return $ N.TVar i
            Nothing -> fail "unbound type variable"
    C.TNameF x -> return $ N.TAlias (N.Global (x ^. name)) -- tmp: TInt
    C.TFunF ts1 t2 -> N.TFun <$> sequence ts1 <*> t2
    C.TExistsF x t -> N.TExists <$> locally varScope (x :) t
    C.TRecursF x t -> N.TRecurs <$> locally varScope (x :) t
    C.TRowF r -> do
        ts <- closureNamelessRow r
        return $ N.TStruct (map (,True) ts)

closureNamelessRow :: C.RowTy -> CtxM [N.Ty]
closureNamelessRow = cata $ \case
    t C.:>$ r -> (:) <$> closureNamelessTy t <*> r
    C.RVarF x -> do
        vsc <- view varScope
        case List.elemIndex x vsc of
            Just i  -> return [N.TVar i]
            Nothing -> fail "unbound type variable"
    C.REmptyF -> return []

closureNamelessLit :: C.Lit -> N.Const
closureNamelessLit (C.LInt i) = N.CInt i

closureNamelessVal :: C.Val -> WriterT [N.Bind] CtxM N.Val
closureNamelessVal (C.VLit l)      = return $ N.VConst (closureNamelessLit l)
closureNamelessVal (C.VVar (x, t)) = do
    vsc <- view varScope
    case List.elemIndex x vsc of
        Just i  -> lift $ N.VVar i <$> closureNamelessTy t
        Nothing -> fail "unbound variable"
closureNamelessVal (C.VFun (f, t)) = do
    fsc <- view funScope
    case lookup f fsc of
        Just g  -> lift $ N.VConst <$> (N.CGlobal g <$> closureNamelessTy t)
        Nothing -> fail "unbound variable"
closureNamelessVal (C.VLabel l _) = do
    labix <- view labelIndex
    case lookup l labix of
        Just i  -> return $ N.VConst (N.CInt i)
        Nothing -> fail "label not found"
closureNamelessVal (C.VTuple vs) = do
    ts <- lift $ mapM (closureNamelessTy . C.typeof) vs
    let bind0 = N.BMalloc ts
        mallocty = N.TStruct (map (,False) ts)
        structtys = map (\i -> N.TStruct (zipWith (\j -> (,j <= i)) [1..] ts)) [(1 :: Int)..]
    binds <- zipWithM (\i -> fmap (N.BUpdate (N.VVar 0 (structtys !! i)) i) . closureNamelessVal) [1..] vs
    tell (bind0 : binds)
    return $ N.VVar 0 (last (mallocty : structtys))
closureNamelessVal (C.VPack t1 v t2) =
    N.VPack <$> lift (closureNamelessTy t1) <*> closureNamelessVal v <*> lift (closureNamelessTy t2)
closureNamelessVal (C.VRoll v t) =
    N.VRoll <$> closureNamelessVal v <*> lift (closureNamelessTy t)
closureNamelessVal (C.VUnroll v) =
    N.VUnroll <$> closureNamelessVal v
closureNamelessVal (C.VAnnot v t) =
    N.VAnnot <$> closureNamelessVal v <*> lift (closureNamelessTy t)

closureNamelessExp :: C.Exp -> CtxM N.Exp
closureNamelessExp (C.ELet (C.BVal x v) e) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    e' <- locally varScope (fst x:) $ closureNamelessExp e
    return $ foldr N.ELet e' (bs ++ [N.BVal v'])
closureNamelessExp (C.ELet (C.BCall x v vs) e) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    (vs', bss) <- mapAndUnzipM (runWriterT . closureNamelessVal) vs
    e' <- locally varScope (fst x:) $ closureNamelessExp e
    return $ foldr N.ELet e' (bs ++ concat bss ++ [N.BCall v' vs'])
closureNamelessExp (C.ELet (C.BProj x v i) e) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    e' <- locally varScope (fst x:) $ closureNamelessExp e
    return $ foldr N.ELet e' (bs ++ [N.BProj v' i])
closureNamelessExp (C.ELet (C.BUnpack tv x v) e) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    e' <- locally varScope ([tv, fst x] ++) $ closureNamelessExp e
    return $ foldr N.ELet e' (bs ++ [N.BUnpack v'])
closureNamelessExp (C.ECase v les) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    ies' <- forM les $ \(l, e) -> do
        labix <- view labelIndex
        case lookup l labix of
            Just i  -> (i,) <$> closureNamelessExp e
            Nothing -> fail "label not found"
    es' <- forM [1..length les] $ \i -> case lookup i ies' of
        Just e  -> return e
        Nothing -> fail "label index not found"
    return $ foldr N.ELet (N.ECase v' es') bs
closureNamelessExp (C.EReturn v) = do
    (v', bs) <- runWriterT $ closureNamelessVal v
    return $ foldr N.ELet (N.EReturn v') bs
closureNamelessExp (C.EAnnot e t) = N.EAnnot <$> closureNamelessExp e <*> closureNamelessTy t

closureNamelessDecs :: [C.Dec] -> WriterT [(N.Global, N.Heap)] CtxM a -> WriterT [(N.Global, N.Heap)] CtxM a
closureNamelessDecs [] m = m
closureNamelessDecs (C.DEnum x ls : decs) m = do
    let g = N.Global (x ^. name)
    tell [(g, N.HTypeAlias N.TInt)]
    tell $ zipWith (\l i -> (N.Global l, N.HVal (N.TAlias g) (N.VConst (N.CInt i)))) ls [1..]
    locally labelIndex (zip ls [1..] ++) $ closureNamelessDecs decs m
closureNamelessDecs (C.DBind x t : decs) m = do
    let g = N.Global (x ^. name)
    t' <- lift $ closureNamelessTy t
    tell [(g, N.HExtern t')]
    closureNamelessDecs decs m

closureNamelessDefns :: [C.Defn] -> WriterT [(N.Global, N.Heap)] CtxM a -> WriterT [(N.Global, N.Heap)] CtxM a
closureNamelessDefns defns m = do
    let fsc = map (\C.Defn{C.code = (f, _)} -> (f, N.Global (f ^. name))) defns
    locally funScope (fsc ++) $ do
        forM_ defns $ \(C.Defn f xs e) -> do
            let g = N.Global (fst f ^. name)
            arg_tys <- lift $ mapM (closureNamelessTy . snd) xs
            ret_ty <- lift $ closureNamelessTy (C.typeof e)
            e' <- lift $ closureNamelessExp e
            tell [(g, N.HCode arg_tys ret_ty e')]
        m

closureNamelessProgram :: C.Program -> IO N.Program
closureNamelessProgram (decs, defns, exp) = runCtxM $ do
    (exp', heaps) <-runWriterT $
        closureNamelessDecs decs $
        closureNamelessDefns defns $
        lift $ closureNamelessExp exp
    return (heaps, exp')
