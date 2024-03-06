module PisigmaTal.Lambda.Check (checkProgram) where

import Control.Monad
import Control.Monad.Reader
import PisigmaTal.Lambda
import PisigmaTal.Lambda.Init

checkEqTys :: Ty -> Ty -> IO ()
checkEqTys TInt TInt = return ()
checkEqTys (TName x) (TName y) | x == y = return ()
checkEqTys (TFun t1 t2) (TFun u1 u2) = do
    checkEqTys t1 u1
    checkEqTys t2 u2
checkEqTys (TTuple ts) (TTuple us) = zipWithM_ checkEqTys ts us
checkEqTys t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkExp :: Exp -> ReaderT Env IO Ty
checkExp (ELit (LInt _)) = return TInt
checkExp (EVar x) = do
    env <- ask
    case lookupBindEnv (fst x) env of
        Just t  -> do
            lift $ checkEqTys (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
checkExp (ELabel _ t) = return t
checkExp (EApp e1 e2) = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    case t1 of
        TFun t u -> do
            lift $ checkEqTys t t2
            return u
        _ -> fail $ "required function type, but got " ++ show t1
checkExp (ELam x e) = do
    t <- local (extendBindEnv x) $ checkExp e
    return $ TFun (snd x) t
checkExp (EExtern fvar args) = do
    arg_tys <- mapM checkExp args
    mb_fun_ty <- asks $ lookupBindEnv (fst fvar)
    case mb_fun_ty of
        Just fun_ty | (arg_tys', ret_ty) <- splitTFun fun_ty -> do
            lift $ zipWithM_ checkEqTys arg_tys' arg_tys
            return ret_ty
        _ -> fail "unknown external function"
checkExp (ELet x e1 e2) = do
    t1 <- checkExp e1
    lift $ checkEqTys (snd x) t1
    local (extendBindEnv x) $ checkExp e2
checkExp (ELetrec xes e2) =
    local (\env -> foldr (extendBindEnv . fst) env xes) $ do
        forM_ xes $ \(x, e) -> do
            t <- checkExp e
            lift $ checkEqTys (snd x) t
        checkExp e2
checkExp (ETuple es) = TTuple <$> mapM checkExp es
checkExp (ECase e les)
    | (_, t1) : _ <- les = do
        ls <- checkExp e >>= \case
            TName x -> lookupEnumEnv x =<< ask
            _       -> fail "TName required"
        guard $ all (\(l, _) -> l `elem` ls) les -- mapbe non-exhaustive
        ts <- mapM (checkExp . snd) les
        t1' <- checkExp t1
        lift $ mapM_ (checkEqTys t1') ts
        return t1'
    | [] <- les = error "empty alternatives"
checkExp (EAnnot e t) = do
    t' <- checkExp e
    lift $ checkEqTys t t'
    return t

checkProgram :: Program -> IO ()
checkProgram (_, e) = void $ runReaderT (checkExp e) initEnv
