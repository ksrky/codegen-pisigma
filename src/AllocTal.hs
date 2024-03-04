{-# LANGUAGE TemplateHaskell #-}

module AllocTal where

import Alloc                    qualified as A
import Tal                      qualified as T

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable
import Data.Map.Strict          qualified as M
import Idx
import Prelude                  hiding (exp)

data Ctx = Ctx {
    _idReg      :: [T.Reg],
    _idFuncName :: [(A.Name, T.Name)],
    _regFileTy  :: T.RegFileTy
}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

allocTalTy :: A.Ty -> CtxM T.Ty
allocTalTy = cata $ \case
    A.TIntF -> return T.TInt
    A.TVarF i -> return $ T.TVar i
    A.TFunF tys _ -> T.TRegFile . T.mkRegFileTy <$> sequence tys
    A.TExistsF ty -> T.TExists <$> ty
    A.TRecursF ty -> T.TRecurs <$> ty
    A.TRowF row -> undefined
    A.TAliasF n  -> undefined

allocConst :: A.Const -> T.WordVal
allocConst (A.CInt i)               = T.VInt i
allocConst (A.CGlobal (A.Name s) _) = T.VLabel (T.Name s)

allocTalVal :: A.Val -> CtxM T.SmallVal
allocTalVal (A.VVar x t) = undefined
allocTalVal (A.VConst c) = return $ T.VWord (allocConst c)
allocTalVal (A.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (A.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (A.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (A.VAnnot v _) = allocTalVal v

allocTalNonVarVal :: A.Val -> CtxM T.WordVal
allocTalNonVarVal = cata $ \case
    A.VVarF{} -> fail "unexpected variable"
    A.VConstF c -> return $ allocConst c
    A.VPackF ty1 val ty2 -> T.VPack <$> allocTalTy ty1 <*> val <*> allocTalTy ty2
    A.VRollF val ty -> T.VRoll <$> val <*> allocTalTy ty
    A.VUnrollF val -> T.VUnroll <$> val
    A.VAnnotF val _ -> val

data St = St {
    _nextReg :: T.Reg,
    _heaps   :: T.Heaps,
    _instrs  :: T.Instrs
}

makeLenses ''St

type StM = StateT St CtxM

execStM :: StM a -> CtxM St
execStM m = execStateT m (St (T.Reg 1) [] (error "invalid termination"))

freshReg :: StM T.Reg
freshReg = do
    r <- use nextReg
    nextReg .= T.Reg (T.unReg r + 1)
    return r

allocTalExp :: A.Exp -> StM ()
allocTalExp (A.ELet (A.BVal ty val) exp) = do
    reg <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- lift $ allocTalVal val
    instrs %= T.ISeq (T.IMove reg val')
allocTalExp (A.ELet (A.BCall ty val vals) exp) | let arity = length vals = do
    reg <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- lift $ allocTalVal val
    vals' <- mapM (lift . allocTalVal) vals
    tmpRegs <- replicateM arity freshReg
    let argRegs = map T.Reg [1 .. arity]
        mv_instrs =
            zipWith T.IMove tmpRegs vals'
            ++ zipWith (\a t -> T.IMove a (T.VReg t)) argRegs tmpRegs
    instrs %= foldr ((.) . T.ISeq) (T.ISeq (T.ICall reg val' argRegs)) mv_instrs
allocTalExp (A.ELet (A.BProj ty val idx) exp) = do
    reg <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- lift $ allocTalVal val
    instrs %= T.ISeq (T.IMove reg val') . T.ISeq (T.ILoad reg reg (idxToInt idx - 1))
allocTalExp (A.ELet (A.BUnpack exty val) exp) | A.TExists ty <- exty = do
    reg <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $ -- tmp: TyVar telescopes
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    val' <- lift $ allocTalVal val
    instrs %= T.ISeq (T.IUnpack reg val') -- tmp: TyVar
allocTalExp (A.ELet A.BUnpack{} _) = error "expected existential type"
allocTalExp (A.ELet (A.BMalloc ty tys) exp) = do
    reg <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    tys' <- mapM (lift . allocTalTy) tys
    instrs %= T.ISeq (T.IMalloc reg tys')
allocTalExp (A.ELet (A.BUpdate ty var idx val) exp) = do
    reg <- freshReg
    reg' <- freshReg
    ty' <- lift $ allocTalTy ty
    locally idReg (reg :) $
        locally regFileTy (M.insert reg ty') $ allocTalExp exp
    var' <- lift $ allocTalVal var
    val' <- lift $ allocTalVal val
    instrs %= T.ISeq (T.IMove reg var') . T.ISeq (T.IMove reg' val') . T.ISeq (T.IStore reg (idxToInt idx) reg')
allocTalExp (A.ECase val cases) = do
    undefined
allocTalExp (A.EReturn val) = do
    val' <- lift $ allocTalVal val
    ty <- lift $ allocTalTy (A.typeof val)
    instrs .= T.ISeq (T.IMove T.retReg val') (T.IHalt ty)
    undefined
allocTalExp (A.EAnnot exp _) = allocTalExp exp

allocTalHeap :: A.Heap -> CtxM T.Heap
allocTalHeap (A.HGlobal _ val) = T.HGlobal <$> allocTalNonVarVal val
allocTalHeap (A.HCode tys _ exp) = do
    rfilety <- T.mkRegFileTy <$> mapM allocTalTy tys
    st <- locally idReg (map T.Reg [1 .. length tys] ++) $
        execStM (allocTalExp exp)
    return $ T.HCode rfilety (st ^. instrs)
allocTalHeap _                 = undefined

allocTalProgram :: A.Program -> CtxM T.Program
allocTalProgram (heaps, exp) = do
    undefined
