module Closure.Tc (tcProg) where

import Closure
import Control.Monad
import Control.Monad.Reader
import Prelude              hiding (exp)

check :: Ty -> Ty -> IO ()
check TInt TInt = return ()
check (TVar x) (TVar y) | x == y = return ()
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

tcVal :: Val -> ReaderT [Var] IO Ty
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
        TEx t2' -> do
            lift $ check (substEx t1 t2') t
            return t2
        _ -> fail $ "expected existential type, but got " ++ show t2
tcVal (VRoll v t) = do
    t' <- tcVal v
    case t of
        TRec t2 -> do
            lift $ check (substRec t t2) t'
            return t
        _ -> fail $ "expected recursive type, but got " ++ show t
tcVal (VUnroll v) = do
    t <- tcVal v
    case t of
        TRec t2 -> return $ substRec t t2
        _       -> fail $ "expected recursive type, but got " ++ show t
tcVal (VValTy v t) = do
    t' <- tcVal v
    lift $ check t t'
    return t

tcDec :: Dec -> ReaderT [Var] IO ()
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
            go 1 (t1 :> _) = t1
            go n (_  :> r) = go (n - 1) r
            go _ _         = error "impossible"
        _ -> fail $ "required row type, but got " ++ show t
tcDec (DUnpack x v2) = do
    t2 <- tcVal v2
    case t2 of
        TEx t -> lift $ check (snd x) t
        _     -> fail $ "required existential type, but got " ++ show t2

tcExp :: Exp -> ReaderT [Var] IO Ty
tcExp (ELet d e) = do
    tcDec d
    local (getDecVar d:) $ tcExp e
tcExp (ERet v) = tcVal v
tcExp (EExpTy e t) = do
    t' <- tcExp e
    lift $ check t t'
    return t

tcDef :: Def -> ReaderT [Var] IO ()
tcDef (Def f xs e) = do
    t <- local (xs ++) $ tcExp e
    lift $ check (snd f) (TFun (map snd xs) t)

tcProg :: Prog -> IO ()
tcProg (defs, exp) = do
    let ctx = map (\Def{code} -> code) defs
    runReaderT (mapM_ tcDef defs >> void (tcExp exp)) ctx
