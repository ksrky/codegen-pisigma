{-# LANGUAGE TemplateHaskell #-}

module ClosureAlloc (closureAllocProgram) where

import Alloc                    qualified as A
import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Foldable
import Data.List                qualified as List
import Id
import Prelude                  hiding (exp)

data Ctx = Ctx {
    _varScope   :: [Id],
    _funScope   :: [(Id, A.Name)],
    _labelIndex :: [(String, Int)]}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

runCtxM :: CtxM a -> IO a
runCtxM m = runReaderT m (Ctx [] [] [])

closureAllocTy :: C.Ty -> CtxM A.Ty
closureAllocTy = cata $ \case
    C.TIntF        -> return A.TInt
    C.TVarF x      -> do
        vsc <- view varScope
        case List.elemIndex x vsc of
            Just i  -> return $ A.TVar i
            Nothing -> fail "unbound type variable"
    C.TNameF x -> return $ A.TAlias (A.Name (x ^. name)) -- tmp: TInt
    C.TFunF ts1 t2 -> A.TFun <$> sequence ts1 <*> t2
    C.TExistsF x t -> A.TExists <$> locally varScope (x :) t
    C.TRecursF x t -> A.TRecurs <$> locally varScope (x :) t
    C.TRowF r -> do
        ts <- closureAllocRow r
        return $ A.TStruct (map (,True) ts)

closureAllocRow :: C.RowTy -> CtxM [A.Ty]
closureAllocRow = cata $ \case
    t C.:>$ r -> (:) <$> closureAllocTy t <*> r
    C.RVarF x -> do
        vsc <- view varScope
        case List.elemIndex x vsc of
            Just i  -> return [A.TVar i]
            Nothing -> fail "unbound type variable"
    C.REmptyF -> return []

closureAllocLit :: C.Lit -> A.Const
closureAllocLit (C.LInt i) = A.CInt i

closureAllocVal :: C.Val -> WriterT [A.Bind] CtxM A.Val
closureAllocVal (C.VLit l)      = return $ A.VConst (closureAllocLit l)
closureAllocVal (C.VVar (x, t)) = do
    vsc <- view varScope
    case List.elemIndex x vsc of
        Just i  -> lift $ A.VVar i <$> closureAllocTy t
        Nothing -> fail "unbound variable"
closureAllocVal (C.VFun (f, t)) = do
    fsc <- view funScope
    case lookup f fsc of
        Just g  -> lift $ A.VConst <$> (A.CGlobal g <$> closureAllocTy t)
        Nothing -> fail "unbound variable"
closureAllocVal (C.VLabel l _) = do
    labix <- view labelIndex
    case lookup l labix of
        Just i  -> return $ A.VConst (A.CInt i)
        Nothing -> fail "label not found"
closureAllocVal (C.VTuple vs) = do
    ts <- lift $ mapM (closureAllocTy . C.typeof) vs
    let bind0 = A.BMalloc ts
        mallocty = A.TStruct (map (,False) ts)
        structtys = map (\i -> A.TStruct (zipWith (\j -> (,j <= i)) [1..] ts)) [(1 :: Int)..]
    binds <- zipWithM (\i -> fmap (A.BUpdate (A.VVar 0 (structtys !! i)) i) . closureAllocVal) [1..] vs
    tell (bind0 : binds)
    return $ A.VVar 0 (last (mallocty : structtys))
closureAllocVal (C.VPack t1 v t2) =
    A.VPack <$> lift (closureAllocTy t1) <*> closureAllocVal v <*> lift (closureAllocTy t2)
closureAllocVal (C.VRoll v t) =
    A.VRoll <$> closureAllocVal v <*> lift (closureAllocTy t)
closureAllocVal (C.VUnroll v) =
    A.VUnroll <$> closureAllocVal v
closureAllocVal (C.VAnnot v t) =
    A.VAnnot <$> closureAllocVal v <*> lift (closureAllocTy t)

closureAllocExp :: C.Exp -> CtxM A.Exp
closureAllocExp (C.ELet (C.BVal x v) e) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    e' <- locally varScope (fst x:) $ closureAllocExp e
    return $ foldr A.ELet e' (bs ++ [A.BVal v'])
closureAllocExp (C.ELet (C.BCall x v vs) e) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    (vs', bss) <- mapAndUnzipM (runWriterT . closureAllocVal) vs
    e' <- locally varScope (fst x:) $ closureAllocExp e
    return $ foldr A.ELet e' (bs ++ concat bss ++ [A.BCall v' vs'])
closureAllocExp (C.ELet (C.BProj x v i) e) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    e' <- locally varScope (fst x:) $ closureAllocExp e
    return $ foldr A.ELet e' (bs ++ [A.BProj v' i])
closureAllocExp (C.ELet (C.BUnpack tv x v) e) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    e' <- locally varScope ([tv, fst x] ++) $ closureAllocExp e
    return $ foldr A.ELet e' (bs ++ [A.BUnpack v'])
closureAllocExp (C.ECase v les) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    ies' <- forM les $ \(l, e) -> do
        labix <- view labelIndex
        case lookup l labix of
            Just i  -> (i,) <$> closureAllocExp e
            Nothing -> fail "label not found"
    es' <- forM [1..length les] $ \i -> case lookup i ies' of
        Just e  -> return e
        Nothing -> fail "label index not found"
    return $ foldr A.ELet (A.ECase v' es') bs
closureAllocExp (C.EReturn v) = do
    (v', bs) <- runWriterT $ closureAllocVal v
    return $ foldr A.ELet (A.EReturn v') bs
closureAllocExp (C.EAnnot e t) = A.EAnnot <$> closureAllocExp e <*> closureAllocTy t

closureAllocDecs :: [C.Dec] -> WriterT [(A.Name, A.Heap)] CtxM a -> WriterT [(A.Name, A.Heap)] CtxM a
closureAllocDecs [] cont = cont
closureAllocDecs (C.DEnum x ls : decs) cont = do
    let g = A.Name (x ^. name)
    tell [(g, A.HTypeAlias A.TInt)]
    tell $ zipWith (\l i -> (A.Name l, A.HVal (A.TAlias g) (A.VConst (A.CInt i)))) ls [1..]
    locally labelIndex (zip ls [1..] ++) $ closureAllocDecs decs cont
closureAllocDecs (C.DBind x t : decs) cont = do
    let g = A.Name (x ^. name)
    t' <- lift $ closureAllocTy t
    tell [(g, A.HExtern t')]
    closureAllocDecs decs cont

closureAllocTopExp :: C.TopExp -> WriterT [(A.Name, A.Heap)] CtxM A.Exp
closureAllocTopExp (defns, exp) = do
    let fsc = map (\((f, _), _) -> (f, A.Name (f ^. name))) defns
    locally funScope (fsc ++) $ do
        forM_ defns $ \(f, C.Code xs e) -> do
            let g = A.Name (fst f ^. name)
            arg_tys <- lift $ mapM (closureAllocTy . snd) xs
            ret_ty <- lift $ closureAllocTy (C.typeof e)
            e' <- lift $ closureAllocExp e
            tell [(g, A.HCode arg_tys ret_ty e')]
        lift $ closureAllocExp exp

closureAllocProgram :: C.Program -> IO A.Program
closureAllocProgram (decs, texp) = runCtxM $ do
    (exp', heaps) <-runWriterT $
        closureAllocDecs decs $
        closureAllocTopExp texp
    return (heaps, exp')
