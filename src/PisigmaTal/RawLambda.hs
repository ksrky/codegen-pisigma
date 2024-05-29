module PisigmaTal.RawLambda (rawLambdaProgram) where

import Control.Lens.Combinators hiding (op)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.IORef
import PisigmaTal.Id
import PisigmaTal.Lambda        qualified as L
import PisigmaTal.Lambda.Init
import PisigmaTal.Raw           qualified as R

type TcM = ReaderT L.Ctx IO

rawLambdaLit :: R.Lit -> L.Lit
rawLambdaLit (R.LInt i) = L.LInt i

newTyVar :: TcM L.Ty
newTyVar = L.TMeta . L.Meta <$> liftIO (newIORef Nothing)

readMeta :: L.Meta -> IO (Maybe L.Ty)
readMeta (L.Meta ref) = readIORef ref

writeMeta :: L.Meta -> L.Ty -> IO ()
writeMeta (L.Meta ref) t = writeIORef ref (Just t)

getMetas :: L.Ty -> [L.Meta]
getMetas = cata $ \case
    L.TIntF -> []
    L.TNameF _ -> []
    L.TFunF t1 t2 -> t1 ++ t2
    L.TTupleF ts -> concat ts
    L.TMetaF m -> [m]

unify :: L.Ty -> L.Ty -> IO ()
unify L.TInt L.TInt = return ()
unify (L.TName x) (L.TName y) | x == y = return ()
unify (L.TFun t1 t2) (L.TFun t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unify (L.TMeta m1) (L.TMeta m2) | m1 == m2 = return ()
unify (L.TMeta m) t = unifyMeta m t
unify t (L.TMeta m) = unifyMeta m t
unify t1 t2 = fail $ "type mismatched. expected " ++ show t1 ++ ", but got " ++ show t2

unifyMeta :: L.Meta -> L.Ty -> IO ()
unifyMeta m1 t2 = do
    mbt1 <- readMeta m1
    case (mbt1, t2) of
        (Just t1, _) -> unify t1 t2
        (Nothing, L.TMeta m2) ->
            readMeta m2 >>= \case
                Just t2' -> unify (L.TMeta m1) t2'
                Nothing -> writeMeta m1 t2
        (Nothing, _) -> do
            occursCheck m1 t2
            writeMeta m1 t2

occursCheck :: L.Meta -> L.Ty -> IO ()
occursCheck tv1 ty2 = do
    let tvs2 = getMetas ty2
    when (tv1 `elem` tvs2) $ fail "occurs checkEqTys failed"

rawLambdaExp :: R.Exp -> TcM L.Exp
rawLambdaExp e = do
    exp_ty <- newTyVar
    e' <- tcExp e exp_ty
    return $ L.EAnnot e' exp_ty

tcExp :: R.Exp -> L.Ty -> TcM L.Exp
tcExp (R.ELit l) exp_ty = do
    lift $ unify exp_ty L.TInt
    return $ L.ELit (rawLambdaLit l)
tcExp (R.EVar x) exp_ty = do
    ctx <- view L.varScope
    case lookup x ctx of
        Just (x', t) -> do
            lift $ unify exp_ty t
            return $ L.EVar (x', t)
        Nothing -> fail $ "unbound variable: " ++ x
tcExp (R.ELabel l) exp_ty = do
    ctx <- view L.labelScope
    case lookup l ctx of
        Just c -> do
            lift $ unify exp_ty (L.TName c)
            return $ L.ELabel c l
        Nothing      -> fail $ "unknown label: " ++ l
tcExp (R.EApp e1 e2) exp_ty = do
    t2 <- newTyVar
    e1' <- tcExp e1 (L.TFun t2 exp_ty)
    e2' <- tcExp e2 t2
    return $ L.EAnnot (L.EApp e1' e2') exp_ty
tcExp (R.ELam x e) exp_ty = do
    x' <- newId x
    t1 <- newTyVar
    t2 <- newTyVar
    lift $ unify exp_ty (L.TFun t1 t2)
    e' <- locally L.varScope ((x, (x', t1)):) $ tcExp e t2
    return $ L.EAnnot (L.ELam (x', t1) e') exp_ty
tcExp (R.EBinOp op e1 e2) exp_ty = do
    ctx <- view L.varScope
    op' <- case lookup op primOps of
        Just (primOp, ty) -> return $ L.PrimOp primOp ty
        Nothing -> case lookup op ctx of
            Just opVar -> return $ uncurry L.KnownOp opVar
            Nothing    -> fail $ "unknown binop: " ++ op
    case L.typeof op' of
        L.TFun t1' (L.TFun t2' tr) -> do
            e1' <- tcExp e1 t1'
            e2' <- tcExp e2 t2'
            lift $ unify exp_ty tr
            return $ L.EAnnot (L.EFullApp op' [e1', e2']) exp_ty
        _ -> fail "required binary function type"
tcExp (R.ELet xes e2) exp_ty = do
    bbs <- forM xes $ \(x, e) -> do
        x' <- newId x
        t <- newTyVar
        e' <- tcExp e t
        return $ L.NonrecBind (x', t) e'
    let env = map (\case L.NonrecBind x _ -> (fst x ^. name, x); _ -> error "impossible") bbs
    e2' <- locally L.varScope (env ++) $ tcExp e2 exp_ty
    return $ L.EAnnot (foldr L.ELet e2' bbs) exp_ty
tcExp (R.ELetrec xes e2) exp_ty = do
    env <- forM xes $ \(x, _) -> do
        x' <- newId x
        tv <- newTyVar
        return (x, (x', tv))
    locally L.varScope (env ++) $ do
        xes' <- zipWithM (\(_, x) (_, e) -> do
            e' <- tcExp e (snd x)
            return (x, e')) env xes
        e2' <- locally L.varScope (env ++) $ tcExp e2 exp_ty
        return $ L.EAnnot (L.ELet (L.MutrecBinds xes') e2') exp_ty
tcExp (R.EIf e1 e2 e3) exp_ty = do
    e1' <- tcExp e1 tyBool
    e2' <- tcExp e2 exp_ty
    e3' <- tcExp e3 exp_ty
    return $ L.EAnnot (L.ECase idBool (L.EAnnot e1' tyBool) [("True", e2'), ("False", e3')]) exp_ty

class Zonking a where
    zonk :: a -> IO a

instance Zonking L.Ty where
    zonk :: L.Ty -> IO L.Ty
    zonk = cata $ \case
        L.TIntF -> return L.TInt
        L.TNameF x -> return $ L.TName x
        L.TFunF t1 t2 -> L.TFun <$> t1 <*> t2
        L.TTupleF ts -> L.TTuple <$> sequence ts
        L.TMetaF m -> readMeta m >>= \case
            Nothing -> return L.TInt -- return $ L.TMeta m
            Just t -> do
                t' <- zonk t
                writeMeta m t'
                -- return t'
                -- tmp: unsolved meta is coerced to TInt. support polymorphism
                case t' of
                    L.TMeta _ -> return L.TInt
                    _         -> return t'

instance Zonking L.Var where
    zonk (x, t) = (x,) <$> zonk t

instance Zonking L.Op where
    zonk = \case
        L.KnownOp x t -> L.KnownOp x <$> zonk t
        L.PrimOp op t -> return $ L.PrimOp op t

instance Zonking L.Exp where
    zonk = cata $ \case
        L.ELitF l -> return $ L.ELit l
        L.EVarF x -> L.EVar <$> zonk x
        L.ELabelF c l -> return $ L.ELabel c l
        L.EAppF e1 e2 -> L.EApp <$> e1 <*> e2
        L.EFullAppF op es -> L.EFullApp <$> zonk op <*> sequence es
        L.ELamF x e -> L.ELam <$> zonk x <*> e
        L.ELetF (L.NonrecBind x e1) e2 -> do
            bb <- L.NonrecBind <$> zonk x <*> zonk e1
            L.ELet bb <$> e2
        L.ELetF (L.MutrecBinds xes) e2 -> do
            bb <- L.MutrecBinds <$> mapM (\(x, e) -> (,) <$> zonk x <*> zonk e) xes
            L.ELet bb <$> e2
        L.ETupleF es -> L.ETuple <$> sequence es
        L.ECaseF c e les -> L.ECase c <$> e <*> mapM (\(l, ei) -> (l,) <$> ei) les
        L.EAnnotF e t -> L.EAnnot <$> e <*> zonk t

rawLambdaProgram :: R.Program -> IO L.Program
rawLambdaProgram raw_prog = do
    e <- zonk =<< runReaderT (rawLambdaExp raw_prog) initCtx
    return (initEnv, e)
