module PisigmaTal.AnfClosure (anfClosureProgram) where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Functor.Foldable
import Data.List                  qualified as List
import Data.Tuple
import PisigmaTal.Anf             qualified as A
import PisigmaTal.Closure         qualified as C
import PisigmaTal.Id
import PisigmaTal.Idx
import Prelude                    hiding (exp)

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = StateT Escapes (ReaderT Locals (WriterT [C.Defn] IO))

findLocals :: C.Var -> CCM ()
findLocals x = do
    lcls <- ask
    escs <- get
    if fst x `elem` lcls || fst x `elem` map fst escs
        then return ()
        else modify (|> x)

removeLocals :: Escapes -> Locals -> Escapes
removeLocals escs lcls = filter (\x -> fst x `notElem` lcls) escs

appendDefn :: C.Defn -> CCM ()
appendDefn = lift . lift . tell . List.singleton

runBindWriter :: Monad m => WriterT [C.Bind] m C.Bind -> m [C.Bind]
runBindWriter m = do
    (x, xs) <- runWriterT m
    return $ xs |> x

anfClosureLit :: A.Lit -> C.Lit
anfClosureLit (A.LInt i) = C.LInt i

anfClosureKnownTy :: A.Ty -> C.Ty
anfClosureKnownTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.TFun ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr C.RSeq C.REmpty ts

anfClosureTy :: A.Ty -> C.Ty
anfClosureTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.ClosTy ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr C.RSeq C.REmpty ts

anfClosureKnownVar :: A.Var -> C.Var
anfClosureKnownVar (x, t) = (x, anfClosureKnownTy t)

anfClosureVar :: A.Var -> C.Var
anfClosureVar (x, t) = (x, anfClosureTy t)

anfClosureVal :: A.Val -> WriterT [C.Bind] CCM C.Val
anfClosureVal (A.VLit l) = return $ C.VLit $ anfClosureLit l
anfClosureVal (A.VVar x) = do
    let x' = anfClosureVar x
    lift $ findLocals x'
    return $ C.VVar x'
anfClosureVal (A.VLabel c l) = return $ C.VLabel c l
anfClosureVal (A.VLam xs exp) = do
        let xs' = map anfClosureVar xs
            lcls = map fst xs
        (exp', escs) <- lift $ runStateT (local (const lcls) $ lift $ anfClosureExp exp) []
        modify $ List.nub . (escs ++)
        let escs' = removeLocals escs lcls
            r_env = foldr (C.RSeq . snd) C.REmpty escs'
            t_env = C.TRow r_env
            t_ucl = C.UClosTy (map snd xs') (C.typeof exp') r_env
            t_cl = C.ClosTy (map snd xs') (C.typeof exp')
            t_code = C.TFun (t_ucl : map snd xs') (C.typeof exp')
        x_cl <- (,t_ucl) <$> newId "x_cl"
        let binds = zipWith (\x i -> C.BProj x (C.VUnroll (C.VVar x_cl)) i) escs' [Idx2 ..]
            v_code = C.Code {
                C.args = x_cl : xs',
                C.body = foldr C.ELet exp' binds
            }
        f_code <- (,t_code) <$> newId "f_code"
        lift $ appendDefn (f_code, v_code)
        f_record <- (,C.TRow (t_code <| r_env)) <$> newId "f_record"
        tell [C.BMalloc f_record (t_code : map snd xs')]
        f_record' <- buildUpdates f_record (C.VFun f_code : map C.VVar escs')
        return $ C.VPack t_env (C.VRoll (C.VVar f_record') t_ucl) t_cl
        -- return $ C.Clos t_env (C.VFun f_code : map C.VVar escs') t_cl
anfClosureVal (A.VTuple vals) = do
    let tys = map (anfClosureTy . A.typeof) vals
        rowty = C.TRow $ foldr C.RSeq C.REmpty tys
    x_tuple <- lift $ (,rowty) <$> newId "x_tuple"
    tell [C.BMalloc x_tuple tys]
    x_tuple' <- go Idx1 x_tuple vals
    return $ C.VVar x_tuple'
  where
    go :: Idx -> C.Var -> [A.Val] -> WriterT [C.Bind] CCM C.Var
    go _ prev [] = return prev
    go idx prev@(x, ty) (val : rest) = do
        next <- (, ty) <$> newId (x ^. name)
        val' <- anfClosureVal val
        tell [C.BUpdate next prev idx val']
        go (IdxS idx) next rest
anfClosureVal (A.VAnnot val ty) = C.VAnnot <$> anfClosureVal val <*> pure (anfClosureTy ty)

buildUpdates :: C.Var -> [C.Val] -> WriterT [C.Bind] CCM C.Var
buildUpdates = go Idx1
  where
    go :: Idx -> C.Var -> [C.Val] -> WriterT [C.Bind] CCM C.Var
    go _ prev [] = return prev
    go idx prev@(x, ty) (val : rest) = do
        next <- (, ty) <$> newId (x ^. name)
        tell [C.BUpdate next prev idx val]
        go (IdxS idx) next rest

anfClosureBind :: A.Bind -> CCM [C.Bind]
anfClosureBind (A.BVal x v) = runBindWriter $ C.BVal (anfClosureVar x) <$> anfClosureVal v
anfClosureBind (A.BApp x v1 vs2)
    | C.TExists tv t_cl <- anfClosureTy (A.typeof v1) = do
    x_cl <- (,t_cl) <$> newId "x_cl"
    binds1 <- runBindWriter $ C.BUnpack tv x_cl <$> anfClosureVal v1
    let t_code = C.TFun (t_cl : map (anfClosureTy . A.typeof) vs2) (anfClosureTy (A.typeof x))
    x_code <- (,t_code) <$> newId "x_code"
    let bind2 = C.BProj x_code (C.VUnroll (C.VVar x_cl)) Idx1
    binds3 <- runBindWriter $ C.BCall (anfClosureVar x) x_code <$> ((C.VVar x_cl :) <$> mapM anfClosureVal vs2)
    return $ binds1 ++ [bind2] ++ binds3
    | otherwise = error "impossible"
anfClosureBind (A.BFullApp x (A.KnownOp f ty) vs2) =
    runBindWriter $ C.BCall (anfClosureVar x) (anfClosureKnownVar (f, ty)) <$> mapM anfClosureVal vs2
anfClosureBind (A.BFullApp x (A.PrimOp op ty) vs2) =
    runBindWriter $ C.BOpCall (anfClosureVar x) op (anfClosureKnownTy ty) <$> mapM anfClosureVal vs2

anfClosureExp :: A.Exp -> CCM C.Exp
anfClosureExp (A.ELet bind body) =
    flip (foldr C.ELet)
        <$> anfClosureBind bind
        <*> local (fst (A.bindVar bind) :) (anfClosureExp body)
anfClosureExp (A.ELetrec binds body) = do
    bindss <- forM binds $ \(A.RecBind f vars exp) -> do
        let f' = anfClosureVar f
        let vars' = map anfClosureVar vars
        (exp', escs) <- lift $ runStateT (local (const $ map fst (f : vars)) $ anfClosureExp exp) []
        modify (escs ++)
        let r_env = foldr (C.RSeq . snd) C.REmpty escs
            t_env = C.TRow r_env
            var_tys = map snd vars'
            ret_ty = C.typeof exp'
            t_ucl = C.UClosTy var_tys ret_ty r_env
            t_cl = C.ClosTy var_tys ret_ty
            t_code = C.TFun (t_ucl : var_tys) ret_ty
        x_cl <- (,t_ucl) <$> newId "x_cl"
        let bind0 = C.BVal f' $ C.VPack t_env (C.VVar x_cl) t_cl
            binds' = zipWith (\x j -> C.BProj x (C.VUnroll (C.VVar x_cl)) j) escs [Idx2 ..]
            v_code = C.Code {
                C.args = x_cl : vars',
                C.body = foldr C.ELet exp' (bind0 : binds')
            }
        f_code <- (,t_code) <$> newId (fst f ^. name  ++ "_code")
        appendDefn (f_code, v_code)
        f_record <- (,C.TRow (t_code <| r_env)) <$> newId (fst f ^. name  ++ "_record")
        let binds1 = [ C.BMalloc f_record (t_code : map snd escs)
                     , C.BVal f' $ C.VPack t_env (C.VRoll (C.VVar f_record) t_ucl) t_cl]
        (_, binds2) <- runWriterT $ buildUpdates f_record (C.VFun f_code : map C.VVar escs)
        return (binds1, binds2)
    modify List.nub
    let (binds1, binds2) = mconcat bindss
    body' <- anfClosureExp body
    return $ foldr C.ELet body' (binds1 ++ binds2)
anfClosureExp (A.ECase c val les) = do
    (val', binds) <- runWriterT $ anfClosureVal val
    body <- C.ECase c val' <$> mapM (\(li, ei) -> (li,) <$> anfClosureExp ei) les
    return $ foldr C.ELet body binds
anfClosureExp (A.EReturn val) = do
    (val', binds) <- runWriterT $ anfClosureVal val
    return $ foldr C.ELet (C.EReturn val') binds
anfClosureExp (A.EAnnot exp ty) = C.EAnnot <$> anfClosureExp exp <*> pure (anfClosureTy ty)

anfClosureDec :: A.Dec -> C.Dec
anfClosureDec (A.DEnum x ls) = C.DEnum x ls
anfClosureDec (A.DBind x t)  = C.DBind x (anfClosureKnownTy t)

anfClosureProgram :: A.Program -> IO C.Program
anfClosureProgram (decs, exp) = do
    (exp', defns) <- runWriterT (runReaderT (evalStateT (anfClosureExp exp) []) [])
    let decs' = map anfClosureDec decs
    return (decs', (defns, exp'))
