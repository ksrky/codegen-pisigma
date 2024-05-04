module PisigmaTal.ClosureTal where

import Control.Lens.Operators
import Control.Monad.Reader
import Data.Functor.Foldable
import PisigmaTal.Closure     qualified as C
import PisigmaTal.Id
import PisigmaTal.Primitive
import Prelude                hiding (exp)
import Tal.Builder
import Tal.Constructors
import Tal.Syntax             qualified as T

allocTalTy :: Monad m => C.Ty -> TalBuilder m T.Ty
allocTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF i -> undefined
    C.TFunF tys _ -> T.TRegFile . mkRegFileTy <$> sequence tys
    C.TExistsF _ ty -> T.TExists <$> ty
    C.TRecursF _ ty -> T.TRecurs <$> ty
    C.TRowF row -> T.TRow <$> allocTalRowTy row
    C.TNameF x -> T.TAlias <$> lookupName undefined -- (x ^. name) -- TODO: freshName wrong

allocTalRowTy :: Monad m => C.RowTy -> TalBuilder m T.RowTy
allocTalRowTy = cata $ \case
    C.REmptyF -> return T.REmpty
    C.RVarF i -> return $ T.RVar undefined -- i
    C.RSeqF ty rest -> T.RSeq <$> allocTalTy ty <*> rest

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

allocTalVal :: Monad m => C.Val -> TalBuilder m T.SmallVal
allocTalVal (C.VLit l) = return $ T.VWord $ closureTalLit l
allocTalVal (C.VVar x) = do
    reg <- findReg undefined -- (x ^. uniq)
    return $ T.VReg reg
allocTalVal (C.VFun x) = do
    reg <- findReg undefined -- (x ^. uniq)
    return $ T.VReg reg
allocTalVal (C.VLabel c l) = undefined
allocTalVal (C.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (C.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (C.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (C.VAnnot v _) = allocTalVal v

allocTalNonVarVal :: MonadFail m => C.Val -> TalBuilder m T.WordVal
allocTalNonVarVal = cata $ \case
    C.VLitF l -> return $ closureTalLit l
    C.VVarF{} -> fail "unexpected variable"
    C.VFunF f ->  undefined
    C.VLabelF c l -> undefined
    C.VPackF ty1 val ty2 -> T.VPack <$> allocTalTy ty1 <*> val <*> allocTalTy ty2
    C.VRollF val ty -> T.VRoll <$> val <*> allocTalTy ty
    C.VUnrollF val -> T.VUnroll <$> val
    C.VAnnotF val _ -> val

mapPrimop :: PrimOp -> T.Aop
mapPrimop = \case Add -> T.Add; Sub -> T.Sub; Mul -> T.Mul; Div -> T.Div

