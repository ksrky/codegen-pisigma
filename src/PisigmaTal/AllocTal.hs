module PisigmaTal.AllocTal where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Map.Strict          qualified as M
import PisigmaTal.Alloc         qualified as A
import PisigmaTal.Idx
import Prelude                  hiding (exp)
import Tal.Constant
import Tal.Constructors
import Tal.Monad
import Tal.Syntax               qualified as T

allocTalTy :: MonadTalCodegen m => A.Ty -> m T.Ty
allocTalTy = cata $ \case
    A.TIntF -> return T.TInt
    A.TVarF i -> return $ T.TVar i
    A.TFunF tys _ -> T.TRegFile . mkRegFileTy <$> sequence tys
    A.TExistsF ty -> T.TExists <$> ty
    A.TRecursF ty -> T.TRecurs <$> ty
    A.TRowF row -> undefined
    A.TAliasF n _ -> undefined

allocConst :: A.Const -> T.WordVal
allocConst (A.CInt i)      = T.VInt i
allocConst (A.CGlobal x _) = T.VLabel undefined

allocTalVal :: MonadTalCodegen m => A.Val -> m T.SmallVal
allocTalVal (A.VVar x t) = undefined
allocTalVal (A.VConst c) = return $ T.VWord (allocConst c)
allocTalVal (A.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (A.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (A.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (A.VAnnot v _) = allocTalVal v

allocTalNonVarVal :: (MonadTalCodegen m, MonadFail m) => A.Val -> m T.WordVal
allocTalNonVarVal = cata $ \case
    A.VVarF{} -> fail "unexpected variable"
    A.VConstF c -> return $ allocConst c
    A.VPackF ty1 val ty2 -> T.VPack <$> allocTalTy ty1 <*> val <*> allocTalTy ty2
    A.VRollF val ty -> T.VRoll <$> val <*> allocTalTy ty
    A.VUnrollF val -> T.VUnroll <$> val
    A.VAnnotF val _ -> val

allocTalExp :: A.Exp -> TalBuilder ()
allocTalExp (A.ELet (A.BVal ty val) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- allocTalVal val
    instrsState %= T.ISeq (T.IMove reg val')
allocTalExp (A.ELet (A.BCall ty val vals) exp) | let arity = length vals = do
    reg <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- allocTalVal val
    vals' <- mapM allocTalVal vals
    tmpRegs <- replicateM arity freshReg
    let mv_instrs =
            zipWith T.IMove tmpRegs vals'
            ++ zipWith (\a t -> T.IMove a (T.VReg t)) argumentRegs tmpRegs
    instrsState %= foldr ((.) . T.ISeq) (T.ISeq (T.ICall reg val' argumentRegs)) mv_instrs
allocTalExp (A.ELet (A.BProj ty val idx) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- allocTalVal val
    instrsState %= T.ISeq (T.IMove reg val') . T.ISeq (T.ILoad reg reg (idxToInt idx - 1))
allocTalExp (A.ELet (A.BUnpack exty val) exp) | A.TExists ty <- exty = do
    reg <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $ -- tmp: TyVar telescopes
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- allocTalVal val
    instrsState %= T.ISeq (T.IUnpack reg val') -- tmp: TyVar
allocTalExp (A.ELet A.BUnpack{} _) = error "expected existential type"
allocTalExp (A.ELet (A.BMalloc ty tys) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    tys' <- mapM allocTalTy tys
    instrsState %= T.ISeq (T.IMalloc reg tys')
allocTalExp (A.ELet (A.BUpdate ty var idx val) exp) = do
    reg <- freshReg
    reg' <- freshReg
    ty' <- allocTalTy ty
    withExtendReg reg $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    var' <- allocTalVal var
    val' <- allocTalVal val
    instrsState %= \ins -> foldr T.ISeq ins [T.IMove reg var', T.IMove reg' val', T.IStore reg (idxToInt idx) reg']
allocTalExp (A.ECase val cases) = do
    undefined
allocTalExp (A.EReturn val) = do
    val' <- allocTalVal val
    ty <- allocTalTy (A.typeof val)
    instrsState .= T.ISeq (T.IMove returnReg val') (T.IHalt ty)
allocTalExp (A.EAnnot exp _) = allocTalExp exp

allocTalHeap :: A.Heap -> TalBuilder T.Heap
allocTalHeap (A.HGlobal _ val) = T.HGlobal <$> allocTalNonVarVal val
allocTalHeap (A.HCode tys _ exp) = do
    rfilety <- mkRegFileTy <$> mapM allocTalTy tys
    st <- withExtendRegs (mkArgumentRegs (length tys)) $
        lift $ runTalBuilderT (allocTalExp exp)
    return $ T.HCode rfilety (st ^. instrsState)
allocTalHeap _ = undefined

allocTalProgram :: A.Program -> IO T.Program
allocTalProgram (heaps, exp) = do
    undefined

-- for each heap
-- StateT (freeRegSet, Instrs)
-- ReaderT (RegFileTy, Telescopes, idReg)

-- ReaderT (idFuncName, idStruct, labelStruct)
