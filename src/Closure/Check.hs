module Closure.Check (checkProgram) where

import Closure

import Control.Monad
import Control.Monad.Reader
import Prelude              hiding (exp)

checkEqTys :: Ty -> Ty -> IO ()
checkEqTys TInt TInt = return ()
checkEqTys (TVar x) (TVar y) | x == y = return ()
checkEqTys (TName x) (TName y) | x == y = return ()
checkEqTys (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ checkEqTys ts1 us1
    checkEqTys t2 u2
checkEqTys (TExists t1) (TExists t2) = checkEqTys t1 t2
checkEqTys (TRecurs t1) (TRecurs t2) = checkEqTys t1 t2
checkEqTys (TRow r1) (TRow r2) = checkEqRowTys r1 r2
checkEqTys t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkEqRowTys :: RowTy -> RowTy -> IO ()
checkEqRowTys REmpty REmpty = return ()
checkEqRowTys (RVar x) (RVar y) | x == y = return ()
checkEqRowTys (t1 :> r1) (t2 :> r2) = do
    checkEqTys t1 t2
    checkEqRowTys r1 r2
checkEqRowTys r1 r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

checkVal :: Val -> ReaderT Env IO Ty
checkVal (VLit (LInt _)) = return TInt
checkVal (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ checkEqTys (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
checkVal (VFun f) = do
    env <- ask
    case lookupBindEnv (fst f) env of
        Just t  -> do
            lift $ checkEqTys (snd f) t
            return t
        Nothing -> fail $ "unbound global: " ++ show f
checkVal (VLabel _ t) = return t
checkVal (VTuple vs) = do
    ts <- mapM checkVal vs
    return $ mkTTuple ts
checkVal (VPack t1 v t2) = do
    t <- checkVal v
    case t2 of
        TExists t2' -> do
            lift $ checkEqTys (unpackClos t1 t2') t
            return t2
        _ -> fail $ "expected existential type, but got " ++ show t2
checkVal (VRoll v t) = do
    t' <- checkVal v
    case t of
        TRecurs t2 -> do
            lift $ checkEqTys (unrollUClos t t2) t'
            return t
        _ -> fail $ "expected recursive type, but got " ++ show t
checkVal (VUnroll v) = do
    t <- checkVal v
    case t of
        TRecurs t2 -> return $ unrollUClos t t2
        _          -> fail $ "expected recursive type, but got " ++ show t
checkVal (VAnnot v t) = do
    t' <- checkVal v
    lift $ checkEqTys t t'
    return t

checkBind :: Bind -> ReaderT Env IO ()
checkBind (BVal x v) = do
    t <- checkVal v
    lift $ checkEqTys (snd x) t
checkBind (BCall x v vs) = do
    t <- checkVal v
    ts <- mapM checkVal vs
    case t of
        TFun ts1 t2 -> lift $ do
            zipWithM_ checkEqTys ts1 ts
            checkEqTys (snd x) t2
        _ -> fail $ "required function type, but got " ++ show t
checkBind (BProj x v i) = do
    t <- checkVal v
    case t of
        TRow row -> do
            lift $ checkEqTys (snd x) (go i row)
          where
            go 1 (t1 :> _) = t1
            go n (_  :> r) = go (n - 1) r
            go _ _         = error "impossible"
        _ -> fail $ "required row type, but got " ++ show t
checkBind (BUnpack x v2) = do
    t2 <- checkVal v2
    case t2 of
        TExists t -> lift $ checkEqTys (snd x) t
        _         -> fail $ "required existential type, but got " ++ show t2

checkExp :: Exp -> ReaderT Env IO Ty
checkExp (ELet b e) = do
    checkBind b
    local (extendBindEnv (bindVar b)) $ checkExp e
checkExp (ECase v les)
    | (_, t1) : _ <- les = do
        ls <- checkVal v >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (checkExp . snd) les
        t1' <- checkExp t1
        lift $ mapM_ (checkEqTys t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
checkExp (EReturn v) = checkVal v
checkExp (EAnnot e t) = do
    t' <- checkExp e
    lift $ checkEqTys t t'
    return t

tcDef :: Defn -> ReaderT Env IO ()
tcDef (Defn f xs e) = do
    t <- local (flip (foldr extendBindEnv) xs) $ checkExp e
    lift $ checkEqTys (snd f) (TFun (map snd xs) t)

checkProgram :: Program -> IO ()
checkProgram (decs, defs, exp) = runReaderT (mapM_ tcDef defs >> void (checkExp exp)) decs
