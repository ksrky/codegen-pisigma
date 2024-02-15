module Anf.Tc (tcProg) where

import Anf
import Control.Monad
import Control.Monad.Reader
import Id

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ check ts1 us1
    check t2 u2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

tcVal :: Val -> ReaderT [(Id, Ty)] IO Ty
tcVal (VLit (LInt _)) = return TInt
tcVal (VVar x) = do
    ctx <- ask
    case lookup (fst x) ctx of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcVal (VLam xs e) = do
    t <- local (xs ++) $ tcExp e
    return $ TFun (map snd xs) t
tcVal (VValTy v t) = do
    t' <- tcVal v
    lift $ check t t'
    return t

tcExp :: Exp -> ReaderT [(Id, Ty)] IO Ty
tcExp (ELet d e) = do
    tcDec d
    local (getDecVar d:) $ tcExp e
tcExp (ELetrec ds e) =
    local (map getDecVar ds ++) $ do
        mapM_ tcDec ds
        tcExp e
tcExp (ERet v) = tcVal v
tcExp (EExpTy e t) = do
    t' <- tcExp e
    lift $ check t t'
    return t

tcDec :: Dec -> ReaderT [(Id, Ty)] IO ()
tcDec (DVal x v) = do
    t <- tcVal v
    lift $ check (snd x) t
tcDec (DCall x v vs) = do
    t <- tcVal v
    ts <- mapM tcVal vs
    case t of
        TFun ts1 t2 -> lift $ do
            zipWithM_ check ts1 ts
            check (snd x) t2
        _ -> fail $ "required function type, but got " ++ show t

tcProg :: Prog -> IO ()
tcProg e = void $ runReaderT (tcExp e) []
