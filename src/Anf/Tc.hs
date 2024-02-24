module Anf.Tc (tcProg) where

import Anf
import Control.Monad
import Control.Monad.Reader

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TName x) (TName y) | x == y = return ()
check (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ check ts1 us1
    check t2 u2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

tcVal :: Val -> ReaderT Env IO Ty
tcVal (VLit (LInt _)) = return TInt
tcVal (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcVal (VLab _ t) = return t
tcVal (VLam xs e) = do
    t <- local (flip (foldr extendBindEnv) xs) $ tcExp e
    return $ TFun (map snd xs) t
tcVal (VValTy v t) = do
    t' <- tcVal v
    lift $ check t t'
    return t

tcExp :: Exp -> ReaderT Env IO Ty
tcExp (ELet b e) = do
    tcBind b
    local (extendBindEnv (bindVar b)) $ tcExp e
tcExp (ELetrec bs e) =
    local (flip (foldr (extendBindEnv . bindVar)) bs) $ do
        mapM_ tcBind bs
        tcExp e
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

tcProg :: Prog -> IO ()
tcProg (decs, e) = void $ runReaderT (tcExp e) decs
