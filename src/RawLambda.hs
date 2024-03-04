module RawLambda (rawLambdaProgram) where

import Id
import Lambda                 qualified as L
import Lambda.Init
import Raw                    qualified as R

import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.IORef

type Ctx = [(String, (Id, L.Ty))]

type TcM = ReaderT Ctx IO

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
    ctx <- ask
    case lookup x ctx of
        Just (x', t) -> do
            lift $ unify exp_ty t
            return $ L.EVar (x', t)
        Nothing -> fail "unbound variable"
tcExp (R.ELabel l) exp_ty = do
    ctx <- ask
    case lookup l ctx of
        Just (x, t) -> do
            lift $ unify exp_ty t
            return $ L.EVar (x, t)
        Nothing      -> fail "unknown label"
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
    e' <- local ((x, (x', t1)):) $ tcExp e t2
    return $ L.EAnnot (L.ELam (x', t1) e') exp_ty
tcExp (R.EBinOp op e1 e2) exp_ty = do
    ctx <- ask
    op' <- case lookup op ctx of
        Just op' -> return op'
        Nothing  -> fail "unknown binop"
    case snd op' of
        L.TFun t1' (L.TFun t2' tr) -> do
            e1' <- tcExp e1 t1'
            e2' <- tcExp e2 t2'
            lift $ unify exp_ty tr
            return $ L.EAnnot (L.EExtern op' [e1', e2']) exp_ty
        _ -> fail "required binary function type"
tcExp (R.ELet xes e2) exp_ty = do
    xes' <- forM xes $ \(x, e) -> do
        x' <- newId x
        t <- newTyVar
        e' <- tcExp e t
        return ((x', t), e')
    e2' <- local (map (\(x, _) -> (fst x ^. name, x)) xes' ++) $ tcExp e2 exp_ty
    return $ L.EAnnot (foldr (uncurry L.ELet) e2' xes') exp_ty
tcExp (R.ELetrec xes e2) exp_ty = do
    env <- forM xes $ \(x, _) -> do
        x' <- newId x
        tv <- newTyVar
        return (x, (x', tv))
    local (env ++) $ do
        xes' <- zipWithM (\(_, x) (_, e) -> do
            e' <- tcExp e (snd x)
            return (x, e')) env xes
        e2' <- local (env ++) $ tcExp e2 exp_ty
        return $ L.EAnnot (L.ELetrec xes' e2') exp_ty
tcExp (R.EIf e1 e2 e3) exp_ty = do
    e1' <- tcExp e1 tyBool
    e2' <- tcExp e2 exp_ty
    e3' <- tcExp e3 exp_ty
    return $ L.EAnnot (L.ECase (L.EAnnot e1' tyBool) [("True", e2'), ("False", e3')]) exp_ty

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

instance Zonking L.Exp where
    zonk = cata $ \case
        L.ELitF l -> return $ L.ELit l
        L.EVarF x -> L.EVar <$> zonk x
        L.ELabelF l t -> L.ELabel l <$> zonk t
        L.EAppF e1 e2 -> L.EApp <$> e1 <*> e2
        L.ELamF x e -> L.ELam <$> zonk x <*> e
        L.EExternF f es -> L.EExtern <$> zonk f <*> sequence es
        L.ELetF x e1 e2 -> L.ELet <$> zonk x <*> e1 <*> e2
        L.ELetrecF xes e2 -> L.ELetrec <$> mapM (\(x, e) -> ((,) <$> zonk x) <*> e) xes <*> e2
        L.ETupleF es -> L.ETuple <$> sequence es
        L.ECaseF e les -> L.ECase <$> e <*> mapM (\(l, ei) -> (l,) <$> ei) les
        L.EAnnotF e t -> L.EAnnot <$> e <*> zonk t

rawLambdaProgram :: R.Program -> IO L.Program
rawLambdaProgram raw_prog = do
    e <- zonk =<< runReaderT (rawLambdaExp raw_prog) initCtx
    return (initEnv, e)
