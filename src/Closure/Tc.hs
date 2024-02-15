module Closure.Tc (tcProg) where

import Closure
import Control.Monad
import Control.Monad.Reader
import Data.Functor.Foldable
import Id

{- check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TVar x) (TVar y) | x == y = return ()
check (TFun ts1 t2) (TFun us1 u2) = do
    zipWithM_ check ts1 us1
    check t2 u2
check (TEx x t1) (TEx _ t2) = check t1 t2
check t1 t2 =
    fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

subst :: TyVar -> Ty -> Ty -> Ty
subst x s = cata $ \case
    TVarF y | x == y -> s
    t -> embed t

tcVal :: Val -> ReaderT [(Id, Ty)] IO Ty
tcVal (VLit (LInt _)) = return TInt
tcVal (VVar x) = do
    ctx <- ask
    case lookup (fst x) ctx of
        Just t  -> do
            lift $ check (snd x) t
            return t
        Nothing -> fail $ "unbound variable: " ++ show x
tcVal (VGlb f) = do
    ctx <- ask
    case lookup (fst f) ctx of
        Just t  -> do
            lift $ check (snd f) t
            return t
        Nothing -> fail $ "unbound global: " ++ show f
tcVal (VTuple vs) = do
    ts <- mapM tcVal vs
    return $ mkTTuple ts
tcVal (VPack t1 v t2) = do
    t <- tcVal v
    case t2 of
        TEx x t2' -> do
            lift $ check (subst x t1 t2') t
            return t2
        _ -> fail $ "expected existential type, but got " ++ show t2
tcVal (VRoll v t) = do
    t' <- tcVal v
    case t of
        TRec x t2 -> do
            lift $ check (subst x t' t2) t'
            return t
        _ -> fail $ "expected recursive type, but got " ++ show t
tcVal (VUnroll v) = do
    t <- tcVal v
    case t of
        TRec x t2 -> return $ subst x t t2
        _         -> fail $ "expected recursive type, but got " ++ show t
tcVal (VValTy v t) = do
    t' <- tcVal v
    lift $ check t t'
    return t

tcExp :: Exp -> ReaderT [(Id, Ty)] IO Ty
tcExp (ELet d e) = do
    tcDec d
    local (getDecVar d:) $ tcExp e
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
tcDec (DProj x v i) = do
    t <- tcVal v
    case t of
        TRow row -> do
            lift $ check (snd x) (go i row)
          where
            go 1 (RSeq t1 _) = t1
            go n (RSeq _ r)  = go (n - 1) r
            go _ _           = error "impossible"
        _ -> fail $ "required tuple type, but got " ++ show t
tcDec (DUnpack tv x v2) = do

    undefined -}

tcProg :: Prog -> IO ()
tcProg = undefined
