module Closure.Tc (tcProg) where

import Closure
import Control.Monad
import Control.Monad.Reader
import Prelude              hiding (exp)

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TVar x) (TVar y) | x == y = return ()
check (TName x) (TName y) | x == y = return ()
check (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ check ts1 us1
    check t2 u2
check (TEx t1) (TEx t2) = check t1 t2
check (TRec t1) (TRec t2) = check t1 t2
check (TRow r1) (TRow r2) = checkRow r1 r2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkRow :: Row -> Row -> IO ()
checkRow REmpty REmpty = return ()
checkRow (RVar x) (RVar y) | x == y = return ()
checkRow (t1 :> r1) (t2 :> r2) = do
    check t1 t2
    checkRow r1 r2
checkRow r1 r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

tcVal :: Val -> ReaderT Env IO Ty
tcVal (VLit (LInt _)) = return TInt
tcVal (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcVal (VGlb f) = do
    env <- ask
    case lookupBindEnv (fst f) env of
        Just t  -> do
            lift $ check (snd f) t
            return t
        Nothing -> fail $ "unbound global: " ++ show f
tcVal (VLab _ t) = return t
tcVal (VTuple vs) = do
    ts <- mapM tcVal vs
    return $ mkTTuple ts
tcVal (VPack t1 v t2) = do
    t <- tcVal v
    case t2 of
        TEx t2' -> do
            lift $ check (unpackClos t1 t2') t
            return t2
        _ -> fail $ "expected existential type, but got " ++ show t2
tcVal (VRoll v t) = do
    t' <- tcVal v
    case t of
        TRec t2 -> do
            lift $ check (unrollUClos t t2) t'
            return t
        _ -> fail $ "expected recursive type, but got " ++ show t
tcVal (VUnroll v) = do
    t <- tcVal v
    case t of
        TRec t2 -> return $ unrollUClos t t2
        _       -> fail $ "expected recursive type, but got " ++ show t
tcVal (VValTy v t) = do
    t' <- tcVal v
    lift $ check t t'
    return t

tcBind :: Bind -> ReaderT Env IO ()
tcBind (BVal x v) = do
    t <- tcVal v
    lift $ check (snd x) t
tcBind (BCall x v vs) = do
    t <- tcVal v
    ts <- mapM tcVal vs
    case t of
        TFun ts1 t2 -> lift $ do
            zipWithM_ check ts1 ts
            check (snd x) t2
        _ -> fail $ "required function type, but got " ++ show t
tcBind (BProj x v i) = do
    t <- tcVal v
    case t of
        TRow row -> do
            lift $ check (snd x) (go i row)
          where
            go 1 (t1 :> _) = t1
            go n (_  :> r) = go (n - 1) r
            go _ _         = error "impossible"
        _ -> fail $ "required row type, but got " ++ show t
tcBind (BUnpack x v2) = do
    t2 <- tcVal v2
    case t2 of
        TEx t -> lift $ check (snd x) t
        _     -> fail $ "required existential type, but got " ++ show t2

tcExp :: Exp -> ReaderT Env IO Ty
tcExp (ELet b e) = do
    tcBind b
    local (extendBindEnv (bindVar b)) $ tcExp e
tcExp (ECase v les)
    | (_, t1) : _ <- les = do
        ls <- tcVal v >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (tcExp . snd) les
        t1' <- tcExp t1
        lift $ mapM_ (check t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
tcExp (ERet v) = tcVal v
tcExp (EExpTy e t) = do
    t' <- tcExp e
    lift $ check t t'
    return t

tcDef :: Def -> ReaderT Env IO ()
tcDef (Def f xs e) = do
    t <- local (flip (foldr extendBindEnv) xs) $ tcExp e
    lift $ check (snd f) (TFun (map snd xs) t)

tcProg :: Prog -> IO ()
tcProg (decs, defs, exp) = runReaderT (mapM_ tcDef defs >> void (tcExp exp)) decs
