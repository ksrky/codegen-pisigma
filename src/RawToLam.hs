module RawToLam (r2lProg) where

import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.IORef
import Id
import Lambda                 qualified as L
import Lambda.Prim
import Raw                    qualified as R

type Ctx = [(String, L.Var)]

type TcM = ReaderT Ctx IO

r2lLit :: R.Lit -> L.Lit
r2lLit (R.LInt i) = L.LInt i

newTyVar :: TcM L.Ty
newTyVar = L.TMeta . L.Meta <$> liftIO (newIORef Nothing)

readMeta :: L.Meta -> IO (Maybe L.Ty)
readMeta (L.Meta ref) = readIORef ref

writeMeta :: L.Meta -> L.Ty -> IO ()
writeMeta (L.Meta ref) t = writeIORef ref (Just t)

getMetas :: L.Ty -> [L.Meta]
getMetas = cata $ \case
    L.TIntF -> []
    L.TFunF t1 t2 -> t1 ++ t2
    L.TMetaF m -> [m]

unify :: L.Ty -> L.Ty -> IO ()
unify L.TInt L.TInt = return ()
unify (L.TFun t1 t2) (L.TFun t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unify (L.TMeta m1) (L.TMeta m2) | m1 == m2 = return ()
unify (L.TMeta m) t = unifyMeta m t
unify t (L.TMeta m) = unifyMeta m t
unify _ _ = fail "unification failed"

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
    when (tv1 `elem` tvs2) $ fail "occurs check failed"

r2lExp :: R.Exp -> TcM L.Exp
r2lExp e = do
    exp_ty <- newTyVar
    e' <- checkExp e exp_ty
    return $ L.EExpTy e' exp_ty

checkExp :: R.Exp -> L.Ty -> TcM L.Exp
checkExp (R.ELit l) exp_ty = do
    lift $ unify exp_ty L.TInt
    return $ L.ELit (r2lLit l)
checkExp (R.EVar x) exp_ty = do
    env <- ask
    case lookup x env of
        Just (x', t) -> do
            lift $ unify exp_ty t
            return $ L.EVar (x', t)
        Nothing -> fail "unbound variable"
checkExp (R.EApp e1 e2) exp_ty = do
    t2 <- newTyVar
    e1' <- checkExp e1 (L.TFun t2 exp_ty)
    e2' <- checkExp e2 t2
    return $ L.EExpTy (L.EApp e1' e2') exp_ty
checkExp (R.ELam x e) exp_ty = do
    x' <- fromString x
    t1 <- newTyVar
    t2 <- newTyVar
    lift $ unify exp_ty (L.TFun t1 t2)
    e' <- local ((x, (x', t1)):) $ checkExp e t2
    return $ L.EExpTy (L.ELam (x', t1) e') exp_ty
checkExp (R.EBinOp op e1 e2) exp_ty = do
    op' <- case lookup op primDict of
        Just op' -> return op'
        Nothing  -> fail "unknown binop"
    case snd op' of
        L.TFun t1' (L.TFun t2' tr) -> do
            e1' <- checkExp e1 t1'
            e2' <- checkExp e2 t2'
            lift $ unify exp_ty tr
            return $ L.EExpTy (L.EApp (L.EApp (L.EVar op') e1') e2') exp_ty -- tmp: (+) (e1, e2)
        _ -> fail "required function type"
checkExp (R.ELet xes e2) exp_ty = do
    xes' <- forM xes $ \(x, e) -> do
        x' <- fromString x
        t <- newTyVar
        e' <- checkExp e t
        return ((x', t), e')
    e2' <- local (map (\(x, _) -> (fst x ^. name, x)) xes' ++) $ checkExp e2 exp_ty
    return $ L.EExpTy (foldr (uncurry L.ELet) e2' xes') exp_ty
checkExp (R.ELetrec xes e2) exp_ty = do
    env <- forM xes $ \(x, _) -> do
        x' <- fromString x
        tv <- newTyVar
        return (x, (x', tv))
    local (env ++) $ do
        xes' <- zipWithM (\(_, x) (_, e) -> do
            e' <- checkExp e (snd x)
            return (x, e')) env xes
        e2' <- local (env ++) $ checkExp e2 exp_ty
        return $ L.EExpTy (L.ELetrec xes' e2') exp_ty

class Zonking a where
    zonk :: a -> IO a

instance Zonking L.Ty where
    zonk :: L.Ty -> IO L.Ty
    zonk = cata $ \case
        L.TIntF -> return L.TInt
        L.TFunF t1 t2 -> L.TFun <$> t1 <*> t2
        L.TMetaF m -> readMeta m >>= \case
            Nothing -> return L.TInt -- return $ L.TMeta m
            Just t -> do
                t' <- zonk t
                writeMeta m t'
                -- return t'
                case t' of
                    L.TMeta _ -> return L.TInt
                    _         -> return t'

instance Zonking L.Var where
    zonk (x, t) = (x,) <$> zonk t

instance Zonking L.Exp where
    zonk = cata $ \case
        L.ELitF l -> return $ L.ELit l
        L.EVarF x -> L.EVar <$> zonk x
        L.EAppF e1 e2 -> L.EApp <$> e1 <*> e2
        L.ELamF x e -> L.ELam <$> zonk x <*> e
        L.ELetF x e1 e2 -> L.ELet <$> zonk x <*> e1 <*> e2
        L.ELetrecF xes e2 -> L.ELetrec <$> mapM (\(x, e) -> ((,) <$> zonk x) <*> e) xes <*> e2
        L.EExpTyF e t -> L.EExpTy <$> e <*> zonk t

r2lProg :: R.Prog -> IO L.Prog
r2lProg r = zonk =<< runReaderT (r2lExp r) []
