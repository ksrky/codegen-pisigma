{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.AllocTal where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable
import Data.Map.Strict          qualified as M
import PisigmaTal.Alloc         qualified as A
import PisigmaTal.Id
import PisigmaTal.Idx
import Prelude                  hiding (exp)
import Tal.Constant
import Tal.Constructors
import Tal.Context
import Tal.Syntax               qualified as T

data TalState = TalState
    { _heapsState :: T.Heaps
    , _heapLabels :: M.Map Id T.Label
    }

makeClassy ''TalState

type TalM m = StateT TalState m

instance MonadTalBuilder m => MonadTalBuilder (TalM m)

initTalState :: TalState
initTalState = TalState
    { _heapsState = M.empty
    , _heapLabels = M.empty
    }

runTalM :: TalM m a -> m (a, TalState)
runTalM builder = runStateT builder initTalState

extendHeaps :: MonadTalBuilder m => [(T.Name, T.Heap)] -> TalM m ()
extendHeaps heaps = heapsState %= M.union (M.fromList heaps)

allocTalTy :: MonadTalBuilder m => A.Ty -> m T.Ty
allocTalTy = cata $ \case
    A.TIntF -> return T.TInt
    A.TVarF i -> return $ T.TVar i
    A.TFunF tys _ -> T.TRegFile . mkRegFileTy <$> sequence tys
    A.TExistsF ty -> T.TExists <$> ty
    A.TRecursF ty -> T.TRecurs <$> ty
    A.TRowF row -> undefined
    A.TAliasF x _ -> undefined

allocTalRowTy :: MonadTalBuilder m => A.RowTy -> m T.RowTy
allocTalRowTy = cata $ \case
    A.REmptyF -> return T.REmpty
    A.RVarF i -> return $ T.RVar i
    (ty, flag) A.:>$ rest -> do
        ty' <- allocTalTy ty
        T.RSeq (ty', flag) <$> rest

allocConst :: A.Const -> T.WordVal
allocConst (A.CInt i)      = T.VInt i
allocConst (A.CGlobal x _) = T.VLabel undefined

allocTalVal :: MonadTalBuilder m => A.Val -> m T.SmallVal
allocTalVal (A.VVar x _ty) = do
    reg <- findReg x
    return $ T.VReg reg
allocTalVal (A.VConst c) = return $ T.VWord (allocConst c)
allocTalVal (A.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (A.VFixPack packs) = undefined
allocTalVal (A.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (A.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (A.VAnnot v _) = allocTalVal v

allocTalNonVarVal :: (MonadTalBuilder m, MonadFail m) => A.Val -> m T.WordVal
allocTalNonVarVal = cata $ \case
    A.VVarF{} -> fail "unexpected variable"
    A.VConstF c -> return $ allocConst c
    A.VPackF ty1 val ty2 -> T.VPack <$> allocTalTy ty1 <*> val <*> allocTalTy ty2
    A.VFixPackF packs -> undefined
    A.VRollF val ty -> T.VRoll <$> val <*> allocTalTy ty
    A.VUnrollF val -> T.VUnroll <$> val
    A.VAnnotF val _ -> val

allocTalExp :: (MonadTalBuilder m, MonadIO m) => A.Exp -> TalM m T.Instrs
allocTalExp (A.ELet (A.BVal ty val) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    val' <- allocTalVal val
    instrs <- withExtendReg reg $
        withExtendRegTy reg ty' $ allocTalExp exp
    return $ T.IMove reg val' <| instrs
allocTalExp (A.ELet (A.BCall ty val vals) exp) | let arity = length vals = do
    reg <- freshReg
    ty' <- allocTalTy ty
    val' <- allocTalVal val
    vals' <- mapM allocTalVal vals
    instrs <- withExtendReg reg $
        withExtendRegTy reg ty' $ allocTalExp exp
    tmpRegs <- replicateM arity freshReg
    let instrs_storeArgs =
            zipWith T.IMove tmpRegs vals'
            ++ zipWith (\a t -> T.IMove a (T.VReg t)) argumentRegs tmpRegs
    return $ instrs_storeArgs <>| T.ICall val' <| T.IMove reg (T.VReg RVReg) <| instrs
allocTalExp (A.ELet (A.BProj ty val idx) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    val' <- allocTalVal val
    instrs <- withExtendReg reg $
        withExtendRegTy reg ty' $ allocTalExp exp
    return $ T.IMove reg val' <| T.ILoad reg reg (idxToInt idx - 1) <| instrs
allocTalExp (A.ELet (A.BUnpack exty val) exp) | A.TExists ty <- exty = do
    reg <- freshReg
    ty' <- allocTalTy ty
    val' <- allocTalVal val
    instrs <- withExtendReg reg $ -- tmp: TyVar telescopes -- tmp: TyVar telescopes
         -- tmp: TyVar telescopes
        withExtendRegTy reg ty' $ allocTalExp exp
    return $ T.IUnpack reg val' <| instrs -- tmp: TyVar
allocTalExp (A.ELet A.BUnpack{} _) = error "expected existential type"
allocTalExp (A.ELet (A.BMalloc ty tys) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    tys' <- mapM allocTalTy tys
    instrs <- withExtendReg reg $
        withExtendRegTy reg ty' $ allocTalExp exp
    return $ T.IMalloc reg tys' <| instrs
allocTalExp (A.ELet (A.BUpdate ty var idx val) exp) = do
    reg <- freshReg
    reg' <- freshReg
    ty' <- allocTalTy ty
    var' <- allocTalVal var
    val' <- allocTalVal val
    instrs <- withExtendReg reg $
        withExtendRegTy reg ty' $ allocTalExp exp
    return $ T.IMove reg var' <| T.IMove reg' val' <| T.IStore reg (idxToInt idx) reg' <| instrs
allocTalExp (A.ECase val cases) = do
    mapM_ allocTalExp cases
    reg <- freshReg
    val' <- allocTalVal val
    rfty <- view regFileTy
    heaps <- forM cases $ \exp -> do
        instrs <- allocTalExp exp
        return $ T.HCode [] rfty instrs
    labs <- mapM (freshName . show) [0 .. length cases - 1]
    extendHeaps $ zip labs heaps
    undefined
allocTalExp (A.EReturn val) = do
    val' <- allocTalVal val
    ty <- allocTalTy (A.typeof val)
    freeAllRegs
    return $ T.IMove RVReg val' <| T.IHalt ty
allocTalExp (A.EAnnot exp _) = allocTalExp exp

allocTalHeap :: (MonadTalBuilder m, MonadFail m, MonadIO m) => A.Heap -> TalM m T.Heap
allocTalHeap (A.HGlobal _ val) = T.HGlobal <$> allocTalNonVarVal val
allocTalHeap (A.HCode tys _ exp) = do
    rfilety <- mkRegFileTy <$> mapM allocTalTy tys
    instrs <- withExtendRegs (mkArgumentRegs (length tys)) $ allocTalExp exp
    return $ T.HCode [] rfilety instrs
allocTalHeap _ = undefined

allocTalInstrs :: (MonadTalBuilder m, MonadFail m, MonadIO m) => A.Program -> TalM m T.Instrs
allocTalInstrs (idheaps, exp) | (ids, heaps) <- unzip idheaps = do
    labs <- mapM (\x -> freshName (x ^. name)) ids
    heapLabels .= M.fromList (zip ids labs)
    heaps' <- mapM allocTalHeap heaps
    instrs <- allocTalExp exp
    extendHeaps $ zip labs heaps'
    return instrs

allocTalProgram :: A.Program -> IO T.Program
allocTalProgram (heaps, exp) = do
    (instrs, st) <- runTalBuilder $ runTalM $ allocTalInstrs (heaps, exp)
    return (st ^. heapsState, instrs)
