module Closure.Check (checkProgram) where

import Closure

import Control.Monad
import Control.Monad.Reader
import Prelude              hiding (exp)

checkEqTys :: [(TyVar, TyVar)] -> Ty -> Ty -> IO ()
checkEqTys _ TInt TInt = return ()
checkEqTys sc (TVar x) (TVar y) | Just y' <- lookup x sc, y == y' = return ()
checkEqTys _ (TName x) (TName y) | x == y = return ()
checkEqTys sc (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ (checkEqTys sc) ts1 us1
    checkEqTys sc t2 u2
checkEqTys sc (TExists tv1 t1) (TExists tv2 t2) = checkEqTys ((tv1, tv2) : sc) t1 t2
checkEqTys sc (TRecurs tv1 t1) (TRecurs tv2 t2) = checkEqTys ((tv1, tv2) : sc) t1 t2
checkEqTys sc (TRow r1) (TRow r2) = checkEqRowTys sc r1 r2
checkEqTys sc t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2 ++ "\n"
        ++ "under the scope:  " ++ show sc

checkEqRowTys :: [(TyVar, TyVar)] -> RowTy -> RowTy -> IO ()
checkEqRowTys _ REmpty REmpty = return ()
checkEqRowTys sc (RVar x) (RVar y) | Just y' <- lookup x sc, y == y' = return ()
checkEqRowTys sc (t1 :> r1) (t2 :> r2) = do
    checkEqTys sc t1 t2
    checkEqRowTys sc r1 r2
checkEqRowTys sc r1 r2 =
    fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2 ++ "\n"
        ++ "under the scope:  " ++ show sc

checkVal :: [(TyVar, TyVar)] -> Val -> ReaderT Env IO Ty
checkVal _ (VLit (LInt _)) = return TInt
checkVal sc (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ checkEqTys sc (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
checkVal sc (VFun f) = do
    env <- ask
    case lookupBindEnv (fst f) env of
        Just t  -> do
            lift $ checkEqTys sc (snd f) t
            return t
        Nothing -> fail $ "unbound global: " ++ show f
checkVal _ (VLabel _ t) = return t
checkVal sc (VTuple vs) = do
    ts <- mapM (checkVal sc) vs
    return $ mkTTuple ts
checkVal sc (VPack t1 v t2) = do
    t <- checkVal sc v
    case t2 of
        TExists _ t2' -> do
            lift $ checkEqTys sc (unpackClos t1 t2') t
            return t2
        _ -> fail $ "expected existential type, but got " ++ show t2
checkVal sc (VRoll v t) = do
    t' <- checkVal sc v
    case t of
        TRecurs _ t2 -> do
            lift $ checkEqTys sc (unrollUClos t t2) t'
            return t
        _ -> fail $ "expected recursive type, but got " ++ show t
checkVal sc (VUnroll v) = do
    t <- checkVal sc v
    case t of
        TRecurs _ t2 -> return $ unrollUClos t t2
        _            -> fail $ "expected recursive type, but got " ++ show t
checkVal sc (VAnnot v t) = do
    t' <- checkVal sc v
    lift $ checkEqTys sc t t'
    return t

checkBind :: [(TyVar, TyVar)] -> Bind -> ReaderT Env IO ()
checkBind sc (BVal x v) = do
    t <- checkVal sc v
    lift $ checkEqTys sc (snd x) t
checkBind sc (BCall x v vs) = do
    t <- checkVal sc v
    ts <- mapM (checkVal sc) vs
    case t of
        TFun ts1 t2 -> lift $ do
            zipWithM_ (checkEqTys sc) ts1 ts
            checkEqTys sc (snd x) t2
        _ -> fail $ "required function type, but got " ++ show t
checkBind sc (BProj x v i) = do
    t <- checkVal sc v
    case t of
        TRow row -> do
            lift $ checkEqTys sc (snd x) (go i row)
          where
            go 1 (t1 :> _) = t1
            go n (_  :> r) = go (n - 1) r
            go _ _         = error "impossible"
        _ -> fail $ "required row type, but got " ++ show t
checkBind sc (BUnpack tv1 x v2) = do
    t2 <- checkVal sc v2
    case t2 of
        TExists tv2 t -> do
            lift $ checkEqTys ((tv1, tv2) : sc) (snd x) t
        _             -> fail $ "required existential type, but got " ++ show t2

checkExp :: [(TyVar, TyVar)] -> Exp -> ReaderT Env IO Ty
checkExp sc (ELet b e) = do
    checkBind sc b
    local (extendBindEnv (bindVar b)) $ checkExp sc e
checkExp sc (ECase v les)
    | (_, t1) : _ <- les = do
        ls <- checkVal sc v >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (checkExp sc . snd) les
        t1' <- checkExp sc t1
        lift $ mapM_ (checkEqTys sc t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
checkExp sc (EReturn v) = checkVal sc v
checkExp sc (EAnnot e t) = do
    t' <- checkExp sc e
    lift $ checkEqTys sc t t'
    return t

tcDef :: Defn -> ReaderT Env IO ()
tcDef (Defn f xs e) = do
    t <- local (flip (foldr extendBindEnv) xs) $ checkExp [] e
    lift $ checkEqTys [] (snd f) (TFun (map snd xs) t)

checkProgram :: Program -> IO ()
checkProgram (decs, defs, exp) = runReaderT (mapM_ tcDef defs >> void (checkExp [] exp)) decs
