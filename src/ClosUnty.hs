module ClosUnty (c2uProg) where

import Closure               qualified as C
import Data.Functor.Foldable
import Id
import Prelude               hiding (exp)
import Untyped               qualified as U

c2uLit :: C.Lit -> U.Lit
c2uLit (C.LInt i) = U.LInt i

c2uVar :: C.Var -> Id
c2uVar (x, _) = x

c2uVal :: C.Val -> U.Val
c2uVal = cata $ \case
    C.VLitF l -> U.VLit $ c2uLit l
    C.VVarF x -> U.VVar $ c2uVar x
    C.VGlbF f -> U.VGlb $ c2uVar f
    C.VTupleF vs -> U.VTuple vs
    C.VPackF _ v _ -> v
    C.VRollF v _ -> v
    C.VUnrollF v -> v
    C.VValTyF v _ -> v

c2uDec :: C.Dec -> U.Dec
c2uDec (C.DVal x v)       = U.DVal (c2uVar x) (c2uVal v)
c2uDec (C.DCall x v1 vs2) = U.DCall (c2uVar x) (c2uVal v1) (map c2uVal vs2)
c2uDec (C.DProj x v i)    = U.DProj (c2uVar x) (c2uVal v) i
c2uDec (C.DUnpack x v)    = U.DVal (c2uVar x) (c2uVal v)

c2uExp :: C.Exp -> U.Exp
c2uExp = cata $ \case
    C.ELetF d e -> U.ELet (c2uDec d) e
    C.ERetF v -> U.ERet (c2uVal v)
    C.EExpTyF e _ -> e

c2uDef :: C.Def -> U.Def
c2uDef (C.Def f xs e) = U.Def (c2uVar f) (map c2uVar xs) (c2uExp e)

c2uProg :: C.Prog -> U.Prog
c2uProg (defs, exp) = (map c2uDef defs, c2uExp exp)
