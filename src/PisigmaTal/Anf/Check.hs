module PisigmaTal.Anf.Check (checkProgram) where

import Control.Monad
import Control.Monad.Reader
import PisigmaTal.Anf
import Prelude              hiding (exp)

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
    local (flip (foldr (extendBindEnv . (\(RecBind x _ _) -> x))) bs) $ do
        mapM_ checkRecBind bs
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
checkBind (BVal var val) = do
    ty <- checkVal val
    lift $ checkEqTys (snd var) ty
checkBind (BApp var fun args) = do
    fun_ty <- checkVal fun
    arg_tys <- mapM checkVal args
    case fun_ty of
        TFun arg_tys' res_ty -> lift $ do
            zipWithM_ checkEqTys arg_tys' arg_tys
            checkEqTys (snd var) res_ty
        _ -> fail $ "required function type, but got " ++ show fun_ty
checkBind (BFullApp var op args) = do
    arg_tys <- mapM checkVal args
    env <- ask
    fun_ty <- case op of
        KnownOp f ty -> do
            ty' <- lookupBindEnv f env
            lift $ checkEqTys ty ty'
            return ty
        PrimOp _ ty  -> return ty
    case fun_ty of
        TFun arg_tys' res_ty -> lift $ do
            zipWithM_ checkEqTys arg_tys' arg_tys
            checkEqTys (snd var) res_ty
        _ -> fail "required function type"

checkRecBind :: RecBind -> ReaderT Env IO ()
checkRecBind (RecBind f vars exp) = do
    ty <- local (flip (foldr extendBindEnv) vars) $ checkExp exp
    lift $ checkEqTys (snd f) (TFun (map snd vars) ty)

checkProgram :: Program -> IO ()
checkProgram (decs, e) = void $ runReaderT (checkExp e) decs
