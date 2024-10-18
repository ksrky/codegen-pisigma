{-# LANGUAGE ViewPatterns #-}
module PisigmaTal.LambdaAnf (lambdaAnfProgram) where

import Data.Functor.Foldable
import PisigmaTal.Anf        qualified as A
import PisigmaTal.Anf.Monad
import PisigmaTal.Id
import PisigmaTal.Lambda     qualified as L
import Prelude               hiding (exp)

lambdaAnfLit :: L.Lit -> A.Lit
lambdaAnfLit (L.LInt i) = A.LInt i

lambdaAnfTy :: MonadFail m => L.Ty -> m A.Ty
lambdaAnfTy = cata $ \case
    L.TIntF       -> return A.TInt
    L.TNameF x    -> return $ A.TName x
    L.TFunF t1 t2 -> A.TFun <$> sequence [t1] <*> t2
    L.TTupleF ts  -> A.TTuple <$> sequence ts
    L.TMetaF _    -> fail "unsolved meta"

lambdaAnfVar :: L.Var -> MonadFail m => m A.Var
lambdaAnfVar (x, t) = (x,) <$> lambdaAnfTy t

lambdaAnfExp :: L.Exp -> (A.Val -> AnfM A.Exp) -> AnfM A.Exp
lambdaAnfExp (L.ELit l) kont = kont $ A.VLit (lambdaAnfLit l)
lambdaAnfExp (L.EVar x) kont = do
    incrIdCount (fst x)
    kont . A.VVar =<< lambdaAnfVar x
lambdaAnfExp (L.ELabel c l) kont = kont $ A.VLabel c l
lambdaAnfExp (L.EApp e1 e2) kont =
    lambdaAnfExp e1 $ \v1 ->
    lambdaAnfExp e2 $ \v2 -> do
    let t_call = case A.typeof v1 of
            A.TFun _ t2 -> t2
            _           -> error "impossible"
    var <- (,t_call) <$> newId "x_call"
    A.ELet (A.BApp var v1 [v2]) <$> extendIdCount (fst var) (kont (A.VVar var))
lambdaAnfExp (L.EFullApp op exps) kont = go [] exps
  where
    go :: [A.Val] -> [L.Exp] -> AnfM A.Exp
    go acc [] = do
        let (arg_tys, ret_ty) = L.splitTFun $ L.typeof op
        var <- (,) <$> newId "x_ext" <*> lambdaAnfTy ret_ty
        ty' <- A.TFun <$> mapM lambdaAnfTy arg_tys <*> lambdaAnfTy ret_ty
        let op' = case op of
                L.KnownOp x _ -> A.KnownOp x ty'
                L.PrimOp p _  -> A.PrimOp p ty'
        A.ELet (A.BFullApp var op' (reverse acc)) <$> kont (A.VVar var)
    go acc (e : rest) = lambdaAnfExp e $ \v -> go (v : acc) rest
lambdaAnfExp (L.ELam x e) kont =
    kont =<< A.VLam <$> sequence [lambdaAnfVar x] <*> extendIdCount (fst x) (lambdaAnfExp e (pure . A.EReturn))
lambdaAnfExp (L.ELet (L.NonrecBind x e1) e2) kont =
    lambdaAnfExp e1 $ \v1 ->
    A.ELet  <$> (A.BVal <$> lambdaAnfVar x <*> pure v1)
            <*> extendIdCount (fst x) (lambdaAnfExp e2 kont)
lambdaAnfExp (L.ELet (L.MutrecBinds xes1) e2) kont = go [] xes1
  where
    buildRecBind :: (A.Var, A.Val) -> A.RecBind
    buildRecBind (f, A.stripAnnotTop -> A.VLam xs e) = A.RecBind f xs e
    buildRecBind _                                 = error "right-hand side of letrec binding"
    go :: [(A.Var, A.Val)] -> [(L.Var, L.Exp)] -> AnfM A.Exp
    go acc [] = A.ELetrec (map buildRecBind acc) <$> lambdaAnfExp e2 kont
    go acc ((x, e) : xes) = do
        x' <- lambdaAnfVar x
        extendIdCount (fst x) $ lambdaAnfExp e $ \v -> go ((x', v) : acc) xes
lambdaAnfExp (L.ETuple es) kont = go [] es
  where
    go :: [A.Val] -> [L.Exp] -> AnfM A.Exp
    go acc []         = kont $ A.VTuple (reverse acc)
    go acc (e : rest) = lambdaAnfExp e $ \v -> go (v : acc) rest
lambdaAnfExp (L.ECase c e les) kont =
    lambdaAnfExp e $ \v ->
    -- [Note] Duplication of continuation is innefficient.
    --        Use join point to avoid this.
    A.ECase c v <$> mapM (\(li, ei) -> (li,) <$> lambdaAnfExp ei kont) les
lambdaAnfExp (L.EAnnot e t) kont = lambdaAnfExp e $ \v -> kont . A.VAnnot v =<< lambdaAnfTy t

lambdaAnfDec :: L.Dec -> AnfM A.Dec
lambdaAnfDec (L.DEnum x ls) = return $ A.DEnum x ls
lambdaAnfDec (L.DBind x ty) = do
    let (arg_tys, ret_ty) = L.splitTFun ty
    A.DBind x <$> (A.TFun <$> mapM lambdaAnfTy arg_tys <*> lambdaAnfTy ret_ty)

lambdaAnfProgram :: L.Program -> IO A.Program
lambdaAnfProgram (decs, exp) =
    runAnfM $ (,) <$> mapM lambdaAnfDec decs <*> extendExternId decs (lambdaAnfExp exp (pure . A.EReturn))
  where
    extendExternId :: [L.Dec] -> AnfM a -> AnfM a
    extendExternId [] m                   = m
    extendExternId (L.DBind x _ : rest) m = extendIdCount x $ extendExternId rest m
    extendExternId (L.DEnum{} : rest) m   = extendExternId rest m
