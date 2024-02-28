module Anf.Tc (checkProgram) where

import Anf
import Control.Monad
import Control.Monad.Reader

checkEqTys :: Ty -> Ty -> IO ()
checkEqTys TInt TInt = return ()
checkEqTys (TName x) (TName y) | x == y = return ()
checkEqTys (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ checkEqTys ts1 us1
    checkEqTys t2 u2
checkEqTys (TTuple ts) (TTuple us) = zipWithM_ checkEqTys ts us
checkEqTys t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkVal :: Val -> ReaderT Env IO Ty
checkVal (VLit (LInt _)) = return TInt
checkVal (VVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ checkEqTys (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
checkVal (VLabel _ t) = return t
checkVal (VLam xs e) = do
    t <- local (flip (foldr extendBindEnv) xs) $ checkExp e
    return $ TFun (map snd xs) t
checkVal (VTuple vs) = TTuple <$> mapM checkVal vs
checkVal (VAnnot v t) = do
    t' <- checkVal v
    lift $ checkEqTys t t'
    return t

checkExp :: Exp -> ReaderT Env IO Ty
checkExp (ELet b e) = do
    checkBind b
    local (extendBindEnv (bindVar b)) $ checkExp e
checkExp (ELetrec bs e) =
    local (flip (foldr (extendBindEnv . bindVar)) bs) $ do
        mapM_ checkBind bs
        checkExp e
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

checkProgram :: Program -> IO ()
checkProgram (decs, e) = void $ runReaderT (checkExp e) decs
