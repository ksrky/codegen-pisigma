module LamToAnf (l2aProg) where

import Anf                   qualified as A
import Data.Functor.Foldable
import Id
import Lambda                qualified as L

l2aLit :: L.Lit -> A.Lit
l2aLit (L.LInt i) = A.LInt i

l2aTy :: L.Ty -> A.Ty
l2aTy = cata $ \case
    L.TIntF       -> A.TInt
    L.TFunF t1 t2 -> A.TFun [t1] t2
    L.TMetaF _    -> A.TInt -- TODO: polymorphism

l2aVar :: L.Var -> A.Var
l2aVar (x, t) = (x, l2aTy t)

l2aExp :: L.Exp -> (A.Val -> A.Exp) -> A.Exp
l2aExp (L.ELit l) kont = kont $ A.VLit (l2aLit l)
l2aExp (L.EVar x) kont = kont $ A.VVar (l2aVar x)
-- TODO: Defunctionalization
-- for example, @(+) 1 2@ should not be curried,
-- and @(+) 1@ should be converted to @\x -> (+) x 1@
l2aExp (L.EApp e1 e2) kont =
    l2aExp e1 $ \v1 ->
    l2aExp e2 $ \v2 ->
    let t_call = case A.typeof v1 of
            A.TFun _ t2 -> t2
            _           -> error "impossible" in
    let x = (localId "x_call", t_call) in -- tmp: localId?
    let body = kont (A.VVar x) in
    A.ELet (A.DCall x v1 [v2]) body
l2aExp (L.ELam x e) kont = kont $ A.VLam [l2aVar x] (l2aExp e A.ERet)
l2aExp (L.ELet x e1 e2) kont =
    l2aExp e1 $ \v1 ->
    A.ELet (A.DVal (l2aVar x) v1) (l2aExp e2 kont)
l2aExp (L.ELetrec xes1 e2) kont = go [] xes1
  where
    go acc [] = A.ELetrec (map (uncurry A.DVal) acc) (l2aExp e2 kont)
    go acc ((x, e) : xes) = l2aExp e $ \v -> go ((l2aVar x, v) : acc) xes
l2aExp (L.EExpTy e t) kont = l2aExp e $ \v -> kont $ A.VValTy v (l2aTy t)

l2aProg :: L.Prog -> A.Prog
l2aProg = flip l2aExp A.ERet
