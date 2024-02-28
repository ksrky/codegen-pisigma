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
    C.VFunF f -> U.VFunc $ c2uVar f
    C.VLabelF l _ -> U.VLabel l
    C.VTupleF vs -> U.VTuple vs
    C.VPackF _ v _ -> v
    C.VRollF v _ -> v
    C.VUnrollF v -> v
    C.VAnnotF v _ -> v

c2uBind :: C.Bind -> U.Bind
c2uBind (C.BVal x v)       = U.BVal (c2uVar x) (c2uVal v)
c2uBind (C.BCall x v1 vs2) = U.BCall (c2uVar x) (c2uVal v1) (map c2uVal vs2)
c2uBind (C.BProj x v i)    = U.BProj (c2uVar x) (c2uVal v) i
c2uBind (C.BUnpack x v)    = U.BVal (c2uVar x) (c2uVal v)

c2uExp :: C.Exp -> U.Exp
c2uExp = cata $ \case
    C.ELetF d e -> U.ELet (c2uBind d) e
    C.ECaseF v les -> U.ECase (c2uVal v) les
    C.EReturnF v -> U.EReturn (c2uVal v)
    C.EAnnotF e _ -> e

c2uDef :: C.Defn -> U.Defn
c2uDef (C.Defn f xs e) = U.Defn (c2uVar f) (map c2uVar xs) (c2uExp e)

c2uProg :: C.Program -> U.Program
c2uProg (_, defs, exp) = (map c2uDef defs, c2uExp exp)
