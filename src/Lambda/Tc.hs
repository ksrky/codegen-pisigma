module Lambda.Tc (tcProg) where

import Control.Monad
import Control.Monad.Reader
import Lambda
import Lambda.Init

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TName x) (TName y) | x == y = return ()
check (TFun t1 t2) (TFun u1 u2) = do
    check t1 u1
    check t2 u2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

tcExp :: Exp -> ReaderT Env IO Ty
tcExp (ELit (LInt _)) = return TInt
tcExp (EVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcExp (ELab _ t) = return t
tcExp (EApp e1 e2) = do
    t1 <- tcExp e1
    t2 <- tcExp e2
    case t1 of
        TFun t u -> do
            lift $ check t t2
            return u
        _ -> fail $ "required function type, but got " ++ show t1
tcExp (ELam x e) = do
    t <- local (extendBindEnv x) $ tcExp e
    return $ TFun (snd x) t
tcExp (ELet x e1 e2) = do
    t1 <- tcExp e1
    lift $ check (snd x) t1
    local (extendBindEnv x) $ tcExp e2
tcExp (ELetrec xes e2) =
    local (\env -> foldr (extendBindEnv . fst) env xes) $ do
        forM_ xes $ \(x, e) -> do
            t <- tcExp e
            lift $ check (snd x) t
        tcExp e2
tcExp (ECase e les)
    | (_, t1) : _ <- les = do
        ls <- tcExp e >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (tcExp . snd) les
        t1' <- tcExp t1
        lift $ mapM_ (check t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
tcExp (EExpTy e t) = do
    t' <- tcExp e
    lift $ check t t'
    return t

tcProg :: Prog -> IO ()
tcProg (_, e) = void $ runReaderT (tcExp e) initEnv
