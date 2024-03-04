module Closure.Check (checkProgram) where

import Closure

import Control.Lens.At
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Prelude                hiding (exp)

checkEqTys :: [(TyVar, TyVar)] -> Ty -> Ty -> IO ()
checkEqTys _ TInt TInt = return ()
checkEqTys _ (TVar x) (TVar y) | x == y = return ()
checkEqTys cts (TVar x) (TVar y) | Just y' <- lookup x cts, y == y' = return ()
checkEqTys _ (TName x) (TName y) | x == y = return ()
checkEqTys cts (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ (checkEqTys cts) ts1 us1
    checkEqTys cts t2 u2
checkEqTys cts (TExists tv1 t1) (TExists tv2 t2) = checkEqTys ((tv1, tv2) : cts) t1 t2
checkEqTys cts (TRecurs tv1 t1) (TRecurs tv2 t2) = checkEqTys ((tv1, tv2) : cts) t1 t2
checkEqTys cts (TRow r1) (TRow r2) = checkEqRowTys cts r1 r2
checkEqTys _ t1 t2 = fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkEqRowTys :: [(TyVar, TyVar)] -> RowTy -> RowTy -> IO ()
checkEqRowTys _ REmpty REmpty = return ()
checkEqRowTys _ (RVar x) (RVar y) | x == y = return ()
checkEqRowTys cts (RVar x) (RVar y) | Just y' <- lookup x cts, y == y' = return ()
checkEqRowTys cts (t1 :> r1) (t2 :> r2) = do
    checkEqTys cts t1 t2
    checkEqRowTys cts r1 r2
checkEqRowTys _ r1 r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

checkVal :: [(TyVar, TyVar)] -> Val -> ReaderT Env IO Ty
checkVal _ (VLit (LInt _)) = return TInt
checkVal cts (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ checkEqTys cts (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
checkVal cts (VFun f) = do
    env <- ask
    case lookupBindEnv (fst f) env of
        Just t  -> do
            lift $ checkEqTys cts (snd f) t
            return t
        Nothing -> fail $ "unbound global: " ++ show f
checkVal _ (VLabel _ t) = return t
checkVal cts (VTuple vs) = do
    ts <- mapM (checkVal cts) vs
    return $ mkTTuple ts
checkVal cts (VPack t1 v t2)
    | TExists{} <- t2 = do
        t <- checkVal cts v
        lift $ checkEqTys cts (unpackClos t1 t2) t
        return t2
    | otherwise = fail $ "expected existential type, but got " ++ show t2
checkVal cts (VRoll v t)
    | TRecurs{} <- t = do
        t' <- checkVal cts v
        lift $ checkEqTys cts (unrollUClos t) t'
        return t
    | otherwise = fail $ "expected recursive type, but got " ++ show t
checkVal cts (VUnroll v) = do
    t <- checkVal cts v
    case t of
        TRecurs{} -> return $ unrollUClos t
        _         -> fail $ "expected recursive type, but got " ++ show t
checkVal cts (VAnnot v t) = do
    t' <- checkVal cts v
    lift $ checkEqTys cts t t'
    return t

checkBind :: [(TyVar, TyVar)] -> Bind -> ReaderT Env IO ()
checkBind cts (BVal x v) = do
    t <- checkVal cts v
    lift $ checkEqTys cts (snd x) t
checkBind cts (BCall x v vs) = do
    t <- checkVal cts v
    ts <- mapM (checkVal cts) vs
    case t of
        TFun ts1 t2 -> lift $ do
            zipWithM_ (checkEqTys cts) ts1 ts
            checkEqTys cts (snd x) t2
        _ -> fail $ "required function type, but got " ++ show t
checkBind cts (BProj x v idx) = do
    t <- checkVal cts v
    case t of
        TRow row -> lift $ checkEqTys cts (snd x) (row ^?! ix idx)
        _        -> fail $ "required row type, but got " ++ show t
checkBind cts (BUnpack tv1 x v2) = do
    t2 <- checkVal cts v2
    case t2 of
        TExists tv2 t -> do
            lift $ checkEqTys ((tv1, tv2) : cts) (snd x) t
        _             -> fail $ "required existential type, but got " ++ show t2

checkExp :: [(TyVar, TyVar)] -> Exp -> ReaderT Env IO Ty
checkExp cts (ELet b e) = do
    checkBind cts b
    local (extendBindEnv (bindVar b)) $ checkExp cts e
checkExp cts (ECase v les)
    | (_, t1) : _ <- les = do
        ls <- checkVal cts v >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (checkExp cts . snd) les
        t1' <- checkExp cts t1
        lift $ mapM_ (checkEqTys cts t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
checkExp cts (EReturn v) = checkVal cts v
checkExp cts (EAnnot e t) = do
    t' <- checkExp cts e
    lift $ checkEqTys cts t t'
    return t

checkTopExp :: TopExp -> ReaderT Env IO Ty
checkTopExp (defns, exp) = do
    let fsc = map fst defns
    local (flip (foldr extendBindEnv) fsc) $ do
        forM_ defns $ \(f, Code{args, body}) -> do
            t <- local (flip (foldr extendBindEnv) args) $ checkExp [] body
            lift $ checkEqTys [] (snd f) (TFun (map snd args) t)
        checkExp [] exp

checkProgram :: Program -> IO ()
checkProgram (decs, texp) = runReaderT (void (checkTopExp texp)) decs
