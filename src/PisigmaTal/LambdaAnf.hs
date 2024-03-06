module PisigmaTal.LambdaAnf (lambdaAnfProgram) where

import Data.Functor.Foldable
import PisigmaTal.Anf        qualified as A
import PisigmaTal.Id
import PisigmaTal.Lambda     qualified as L
import Prelude               hiding (exp)

-- TODO: optimization
-- curried functions may create unnecessary closures

lambdaAnfLit :: L.Lit -> A.Lit
lambdaAnfLit (L.LInt i) = A.LInt i

lambdaAnfTy :: L.Ty -> A.Ty
lambdaAnfTy = cata $ \case
    L.TIntF       -> A.TInt
    L.TNameF x    -> A.TName x
    L.TFunF t1 t2 -> A.TFun [t1] t2
    L.TTupleF ts  -> A.TTuple ts
    L.TMetaF _    -> error "unsolved meta"

lambdaAnfVar :: L.Var -> A.Var
lambdaAnfVar (x, t) = (x, lambdaAnfTy t)

lambdaAnfExp :: L.Exp -> (A.Val -> A.Exp) -> A.Exp
lambdaAnfExp (L.ELit l) kont = kont $ A.VLit (lambdaAnfLit l)
lambdaAnfExp (L.EVar x) kont = kont $ A.VVar (lambdaAnfVar x)
lambdaAnfExp (L.ELabel l t) kont = kont $ A.VLabel l (lambdaAnfTy t)
lambdaAnfExp (L.EApp e1 e2) kont =
    lambdaAnfExp e1 $ \v1 ->
    lambdaAnfExp e2 $ \v2 ->
    let t_call = case A.typeof v1 of
            A.TFun _ t2 -> t2
            _           -> error "impossible" in
    let var = (newIdUnsafe "x_call", t_call) in
    let body = kont (A.VVar var) in
    A.ELet (A.BCall var (A.LocalFun v1) [v2]) body
lambdaAnfExp (L.ELam x e) kont = kont $ A.VLam [lambdaAnfVar x] (lambdaAnfExp e A.EReturn)
lambdaAnfExp (L.EExtern fvar exps) kont =
    let go :: [A.Val] -> [L.Exp] -> A.Exp
        go acc [] =
            let (arg_tys, ret_ty) = L.splitTFun (snd fvar)
                var = (newIdUnsafe "x_ext", lambdaAnfTy ret_ty)
                body = kont (A.VVar var)
                fvar' = (fst fvar, A.TFun (map lambdaAnfTy arg_tys) (lambdaAnfTy ret_ty))
            in A.ELet (A.BCall var (A.ExternalFun fvar') (reverse acc)) body
        go acc (e : rest) = lambdaAnfExp e $ \v -> go (v : acc) rest
    in go [] exps
lambdaAnfExp (L.ELet x e1 e2) kont =
    lambdaAnfExp e1 $ \v1 ->
    A.ELet (A.BVal (lambdaAnfVar x) v1) (lambdaAnfExp e2 kont)
lambdaAnfExp (L.ELetrec xes1 e2) kont = go [] xes1
  where
    go :: [(A.Var, A.Val)] -> [(L.Var, L.Exp)] -> A.Exp
    go acc [] = A.ELetrec (map (uncurry A.BVal) acc) (lambdaAnfExp e2 kont)
    go acc ((x, e) : xes) = lambdaAnfExp e $ \v -> go ((lambdaAnfVar x, v) : acc) xes
lambdaAnfExp (L.ETuple es) kont = go [] es
  where
    go :: [A.Val] -> [L.Exp] -> A.Exp
    go acc []         = kont $ A.VTuple (reverse acc)
    go acc (e : rest) = lambdaAnfExp e $ \v -> go (v : acc) rest
lambdaAnfExp (L.ECase e les) kont =
    lambdaAnfExp e $ \v ->
    -- [Note] Duplication of continuation is innefficient.
    --        Use join point to avoid this.
    A.ECase v $ map (\(li, ei) -> (li, lambdaAnfExp ei kont)) les
lambdaAnfExp (L.EAnnot e t) kont = lambdaAnfExp e $ \v -> kont $ A.VAnnot v (lambdaAnfTy t)

lambdaAnfDec :: L.Dec -> A.Dec
lambdaAnfDec (L.DEnum x ls) = A.DEnum x ls
lambdaAnfDec (L.DBind x ty)  =
    let (arg_tys, ret_ty) = L.splitTFun ty
    in A.DBind x (A.TFun (map lambdaAnfTy arg_tys) (lambdaAnfTy ret_ty))

lambdaAnfProgram :: L.Program -> A.Program
lambdaAnfProgram (decs, exp) = (map lambdaAnfDec decs, lambdaAnfExp exp A.EReturn)
