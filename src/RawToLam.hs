module RawToLam (r2lProg) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Id
import Lambda                qualified as L
import Lambda.Prim
import Raw                   qualified as R

type Ctx = [(String, L.Var)]

type TcM = ReaderT Ctx

r2lLit :: R.Lit -> L.Lit
r2lLit (R.LInt i) = L.LInt i

r2lTy :: R.Ty -> L.Ty
r2lTy = cata $ \case
    R.TIntF       -> L.TInt
    R.TFunF t1 t2 -> L.TFun t1 t2

r2lExp :: MonadFail m => R.Exp -> TcM m (L.Exp, L.Ty)
r2lExp (R.ELit l) = return (L.ELit (r2lLit l), L.TInt)
r2lExp (R.EVar x) = do
    env <- ask
    case lookup x env of
        Just (x', t) -> return (L.EVar (x', t), t)
        Nothing      -> fail "unbound variable"
r2lExp (R.EApp e1 e2) = do
    (e1', t1) <- r2lExp e1
    (e2', t2) <- r2lExp e2
    case t1 of
        L.TFun t2' t -> if t2 == t2'
            then return (L.EApp (L.EExpTy e1' t1) (L.EExpTy e2' t2), t)
            else fail "type mismatch"
        _ -> fail "required function type"
r2lExp (R.ELam x t e) = do
    let x' = fromString x
    let t1 = r2lTy t
    local ((x, (x', t1)):) $ do
        (e', t2) <- r2lExp e
        return (L.ELam (x', t1) (L.EExpTy e' t2), L.TFun t1 t2)
r2lExp (R.EBinOp op e1 e2) = do
    op' <- case lookup op primDict of
        Just op' -> return op'
        Nothing  -> fail "unknown binop"
    (e1', t1) <- r2lExp e1
    (e2', t2) <- r2lExp e2
    case snd op' of
        L.TFun t1' (L.TFun t2' tr) -> if t1 == t1' && t2 == t2'
            then return (L.EApp (L.EApp (L.EVar op') (L.EExpTy e1' t1)) (L.EExpTy e2' t2), tr)
            else fail "type mismatch"
        _ -> fail "required function type"
r2lExp (R.ELet x e1 e2) = do
    let x' = fromString x
    (e1', t1) <- r2lExp e1
    (e2', t2) <- r2lExp e2
    return (L.ELet (x', t1) (L.EExpTy e1' t1) (L.EExpTy e2' t2), t2)
r2lExp (R.ELetrec xes e2) = do
    xes' <- forM xes $ \(x, e) -> do
        let x' = fromString x
        (e', t) <- r2lExp e
        return ((x', t), L.EExpTy e' t)
    (e2', t2) <- r2lExp e2
    return (L.ELetrec xes' (L.EExpTy e2' t2), t2)

r2lProg :: MonadFail m => R.Prog -> m L.Prog
r2lProg r = fst <$> runReaderT (r2lExp r) []
