{-# LANGUAGE TemplateHaskell #-}

module Alloc.Check (checkProgram) where

import Alloc
import Control.Lens.At
import Control.Lens.Combinators hiding (Const)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Idx
import Prelude                  hiding (exp)

checkEqTys :: Ty -> Ty -> IO ()
checkEqTys TInt TInt = return ()
checkEqTys (TVar x) (TVar y) | x == y = return ()
checkEqTys (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ checkEqTys ts1 us1
    checkEqTys t2 u2
checkEqTys (TExists t1) (TExists t2) = checkEqTys t1 t2
checkEqTys (TRecurs t1) (TRecurs t2) = checkEqTys t1 t2
checkEqTys (TRow r1) (TRow r2) = checkEqRowTys r1 r2
checkEqTys (TAlias x _) (TAlias y _) | x == y = return ()
checkEqTys (TAlias _ (Just ty1)) ty2 = checkEqTys ty1 ty2
checkEqTys ty1 (TAlias _ (Just ty2)) = checkEqTys ty1 ty2
checkEqTys t1 t2 = fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

checkEqRowTys :: RowTy -> RowTy -> IO ()
checkEqRowTys REmpty REmpty = return ()
checkEqRowTys (RVar x) (RVar y) | x == y = return ()
checkEqRowTys ((t1, flag1) :> r1) ((t2, flag2) :> r2) | flag1 == flag2 = do
    checkEqTys t1 t2
    checkEqRowTys r1 r2
checkEqRowTys r1 r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

data Env = Env {
    _localEnv  :: [Maybe Ty], -- Nothing means type variable
    _globalEnv :: [(Name, Ty)]
}

makeLenses ''Env

checkConst :: Const -> ReaderT Env IO Ty
checkConst (CInt _)       = return TInt
checkConst (CGlobal x ty) = do
    Just ty' <- views globalEnv (lookup x)
    lift $ checkEqTys ty ty'
    return ty

checkVal :: Val -> ReaderT Env IO Ty
checkVal (VVar i ty) = do
    Just ty' <- views localEnv (!! i)
    lift $ checkEqTys ty ty'
    return ty
checkVal (VConst c) = checkConst c
checkVal (VPack ty1 val ann_ty)
    | TExists ty2 <- ann_ty = do
        ty <- checkVal val
        lift $ checkEqTys (substTop ty1 ty2) ty
        return ann_ty
    | otherwise = fail $ "expected existential type, but got " ++ show ann_ty
checkVal (VRoll val ann_ty)
    | TRecurs ty <- ann_ty = do
        ty' <- checkVal val
        lift $ checkEqTys (substTop ann_ty ty) ty'
        return ann_ty
    | otherwise = fail $ "expected recursive type, but got " ++ show ann_ty
checkVal (VUnroll val) = do
    ty <- checkVal val
    case ty of
        TRecurs ty' -> return ty'
        _           -> fail $ "expected recursive type, but got " ++ show ty
checkVal (VAnnot val ann_ty) = do
    ty <- checkVal val
    lift $ checkEqTys ann_ty ty
    return ann_ty

checkExp :: Exp -> ReaderT Env IO Ty
checkExp (ELet (BVal ann_ty val) body) = do
    ty <- checkVal val
    lift $ checkEqTys ann_ty ty
    locally localEnv (Just ann_ty :) $ checkExp body
checkExp (ELet (BCall ann_ty val args) body) = do
    ty <- checkVal val
    arg_tys <- mapM checkVal args
    case ty of
        TFun arg_tys' ret_ty -> lift $ do
            zipWithM_ checkEqTys arg_tys' arg_tys
            checkEqTys ann_ty ret_ty
        _ -> fail $ "required function type, but got " ++ show ty
    locally localEnv (Just ann_ty :) $ checkExp body
checkExp (ELet (BProj ann_ty val idx) body) = do
    ty <- checkVal val
    case ty of
        TRow row -> lift $ checkEqTys ann_ty =<< go idx row
        _        -> fail $ "expected tuple type, but got " ++ show ty
    locally localEnv (Just ann_ty :) $ checkExp body
  where
    go :: Idx -> RowTy -> IO Ty
    go Idx1 ((ty1, initialized) :> _)
        | initialized     = return ty1
        | not initialized = fail "field uninitialized"
    go i (_  :> row) = go (idxPred i) row
    go _ _ = error "impossible"
checkExp (ELet (BUnpack ann_ty val) body) = do
    ty <- checkVal val
    case ty of
        TExists ty' -> lift $ checkEqTys ann_ty ty'
        _           -> fail $ "expected existential type, but got " ++ show ty
    locally localEnv ([Nothing , Just ann_ty] ++) $ checkExp body
checkExp (ELet (BMalloc ann_ty tys) body) = do
    let row_ty = TRow $ foldr (\ty -> ((ty, False) :>)) REmpty tys
    lift $ checkEqTys ann_ty row_ty
    locally localEnv (Just ann_ty :) $ checkExp body
checkExp (ELet (BUpdate ann_ty val1 idx val2) body) = do
    ty1 <- checkVal val1
    ty2 <- checkVal val2
    case ty1 of
        TRow row1 -> lift $  do
            checkEqTys (fst (row1 ^?! ix idx)) ty2
            let row1' = row1 & ix idx %~ \(ty, _) -> (ty, True)
            checkEqTys ann_ty (TRow row1')
        _ -> fail $ "expected row type, but got " ++ show ty1
    locally localEnv (Just ann_ty :) $ checkExp body
checkExp (ECase val exps) = do
    ty <- checkVal val
    tys <- mapM checkExp exps
    forM_ tys $ \ty' -> lift $ checkEqTys ty ty'
    return ty
checkExp (EReturn val) = checkVal val
checkExp (EAnnot exp ty) = do
    ty' <- checkExp exp
    lift $ checkEqTys ty ty'
    return ty

checkHeap :: Heap -> ReaderT Env IO ()
checkHeap (HGlobal ty val) = do
    ty' <- checkVal val
    lift $ checkEqTys ty ty'
checkHeap (HCode arg_tys ret_ty exp) = do
    ty <- locally localEnv (\env -> foldl (\e ty -> Just ty : e) env arg_tys) $ checkExp exp
    lift $ checkEqTys ret_ty ty
checkHeap HExtern{} = return ()
checkHeap HTypeAlias{} = return ()

initEnv :: [(Name, Heap)] -> [(Name, Ty)]
initEnv []                                 = []
initEnv ((x, HGlobal ty _) : hs)           = (x, ty) : initEnv hs
initEnv ((x, HCode arg_tys ret_ty _) : hs) = (x, TFun arg_tys ret_ty) : initEnv hs
initEnv ((x, HExtern ty) : hs)             = (x, ty) : initEnv hs
initEnv ((_, HTypeAlias{}) : hs)           = initEnv hs


checkProgram :: Program -> IO ()
checkProgram (heaps, exp) = do
    let env = Env {_localEnv = [], _globalEnv = initEnv heaps}
    runReaderT (mapM_ (checkHeap . snd) heaps >> void (checkExp exp)) env
