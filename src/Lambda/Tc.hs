module Lambda.Tc (tcProg) where

import Control.Monad
import Control.Monad.Reader
import Id
import Lambda

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TFun t1 t2) (TFun u1 u2) = do
    check t1 t2
    check u1 u2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

tcExp :: Exp -> ReaderT [(Id, Ty)] IO Ty
tcExp (ELit (LInt _)) = return TInt
tcExp (EVar x) = do
    ctx <- ask
    case lookup (fst x) ctx of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcExp (EApp e1 e2) = do
    t1 <- tcExp e1
    t2 <- tcExp e2
    case t1 of
        TFun t u -> do
            lift $ check t t2
            return u
        _ -> fail $ "required function type, but got " ++ show t1
tcExp (ELam x e) = do
    t <- local (x:) $ tcExp e
    return $ TFun (snd x) t
tcExp (ELet x e1 e2) = do
    t1 <- tcExp e1
    lift $ check (snd x) t1
    local (x:) $ tcExp e2
tcExp (ELetrec xes e2) =
    local (map fst xes ++) $ do
        forM_ xes $ \(x, e) -> do
            t <- tcExp e
            lift $ check (snd x) t
        tcExp e2
tcExp (EExpTy e t) = do
    t' <- tcExp e
    lift $ check t t'
    return t

tcProg :: Prog -> IO ()
tcProg e = void $ runReaderT (tcExp e) []
