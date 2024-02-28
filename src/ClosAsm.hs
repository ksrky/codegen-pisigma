module ClosAsm where

import Asm                  qualified as A
import Closure              qualified as C
import Control.Monad.State
import Control.Monad.Writer
import Id

data AsmState = AsmState
    { typeMap     :: [(Id, A.Ty)]
    , localRegs   :: [(Id, Int)]
    , globalNames :: [(Id, A.Name)]
    }

type AsmM = StateT AsmState

c2aTy :: C.Ty -> A.Ty
c2aTy C.TInt          = A.TInt 32
c2aTy (C.TVar _)      = A.TPtr Nothing
c2aTy (C.TName x)     = undefined
c2aTy (C.TFun ts1 t2) = A.TPtr $ Just $ A.TFun (c2aTy t2) (map c2aTy ts1)
c2aTy (C.TEx t)       = c2aTy t
c2aTy (C.TRec t)      = c2aTy t
c2aTy (C.TRow r)      = A.TStruct Nothing $ linearRow r

linearRow :: C.Row -> [A.Ty]
linearRow C.REmpty   = []
linearRow (C.RVar _) = [A.TPtr Nothing]
linearRow (t C.:> r) = c2aTy t : linearRow r

c2aLit :: C.Lit -> A.Lit
c2aLit (C.LInt i) = A.LInt 32 (toInteger i)

c2aVal :: C.Val -> Writer [A.Stm] A.Val
c2aVal (C.VLit l)        = return $ A.VLit $ c2aLit l
c2aVal (C.VVar x)        = undefined
c2aVal (C.VGlb f)        = undefined
c2aVal (C.VLab l t)      = case t of
    C.TName x -> undefined
    _         -> undefined
c2aVal (C.VTuple vs)     = undefined
c2aVal (C.VPack _ v _)   = c2aVal v
c2aVal (C.VRoll v _)     = c2aVal v
c2aVal (C.VUnroll v)     = c2aVal v
c2aVal (C.VValTy v _)    = c2aVal v
