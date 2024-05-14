{-# LANGUAGE TemplateHaskell #-}

module Tal.Builder
    ( HasBuilderState(..)
    , HasBuilderContext(..)
    , TalBuilderT
    , runTalBuilderT
    , initBuilderContext
    , initBuilderState
    , extendHeap
    , extendHeaps
    , extInstr
    , buildInstrs
    , buildMove
    , buildMoveWithDst
    , buildStores
    , buildLoads
    , withExtRegTable
    , withExtRegFileTy
    , findReg
    , freshReg
    , setRegInUse
    , setRegFree
    , getInUseRegs
    , isFreeReg
    , newName
    , lookupName
    , withExtTyVarScope
    , getTyVar
    , resetUseCount
    , incUseCount
    , decUseCount
    , isUseCountZero
    , whenUseCountZero
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap              qualified as IM
import Data.List                qualified as L
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Data.Unique
import GHC.Stack
import Tal.Constant
import Tal.Constructors
import Tal.Syntax

data BuilderState = BuilderState
    { _instrStack :: [Instr]
    , _heapsState :: Heaps
    , _freeRegSet :: S.Set Reg
    , _nextUniq   :: Word
    , _nameTable  :: IM.IntMap Name
    , _useCount   :: IM.IntMap Int
    }

makeClassy ''BuilderState

data BuilderContext = BuilderContext
    { _regTable   :: IM.IntMap Reg
    , _regFileTy  :: RegFileTy
    , _tyVarScope :: [Unique]
    , _quants     :: Quants
    }

makeClassy ''BuilderContext

type TalBuilderT m = ReaderT BuilderContext (StateT BuilderState m)

runTalBuilderT :: Monad m => BuilderContext -> BuilderState -> TalBuilderT m a -> m a
runTalBuilderT c s b = evalStateT (runReaderT b c) s

initBuilderContext :: BuilderContext
initBuilderContext = BuilderContext
    { _regTable   = IM.empty
    , _regFileTy  = emptyRegFileTy
    , _tyVarScope = []
    , _quants     = []
    }

initBuilderState :: BuilderState
initBuilderState = BuilderState
    { _instrStack   = []
    , _heapsState   = M.empty
    , _freeRegSet   = initialRegSet
    , _nextUniq     = 0
    , _nameTable    = IM.empty
    , _useCount     = IM.empty
    }

-- ** Heaps

extendHeap :: Monad m => Name -> Heap -> TalBuilderT m ()
extendHeap name heap = heapsState %= M.insert name heap

extendHeaps :: Monad m => [(Name, Heap)] -> TalBuilderT m ()
extendHeaps heaps = heapsState %= M.union (M.fromList heaps)

-- ** Instrs

{-# INLINE extInstr #-}
extInstr :: Monad m => Instr -> TalBuilderT m ()
extInstr instr = instrStack %= (instr :)

popInstrs :: Monad m => TalBuilderT m [Instr]
popInstrs = do
    instrs <- use instrStack
    instrStack .= []
    return instrs

buildInstrs :: Monad m => Instrs -> TalBuilderT m Instrs
buildInstrs term = foldl (flip ISeq) term <$> popInstrs

buildMove :: Monad m => SmallVal -> TalBuilderT m Reg
buildMove (VReg reg) = do
    yes_free <- isFreeReg reg
    if yes_free then return reg
    else buildMove' (VReg reg)
buildMove val = buildMove' val

buildMove' :: Monad m => SmallVal -> TalBuilderT m Reg
buildMove' val = do
    reg <- freshReg
    extInstr $ IMove reg val
    return reg

buildMoveWithDst :: Monad m => Reg -> SmallVal -> TalBuilderT m ()
buildMoveWithDst reg (VReg reg') | reg == reg' = return ()
buildMoveWithDst reg val = extInstr $ IMove reg val

buildStackOp :: Monad m => (Int -> Reg -> Instr) -> [Reg] -> TalBuilderT m ()
buildStackOp _ [] = return ()
buildStackOp stld (r : rs) = do
        extInstr $ stld (length rs) r
        buildStackOp stld rs

buildStores :: Monad m => [Reg] -> TalBuilderT m ()
buildStores [] = return ()
buildStores regs = do
    extInstr $ ISalloc (length regs)
    buildStackOp (ISstore SPReg) regs

buildLoads :: Monad m => [Reg] -> TalBuilderT m ()
buildLoads [] = return ()
buildLoads regs = do
    buildStackOp (\i r -> ISload r SPReg i) regs
    extInstr $ ISfree (length regs)

-- ** Regs

withExtRegTable :: Monad m => Unique -> Reg -> TalBuilderT m a -> TalBuilderT m a
withExtRegTable u reg = locally regTable (IM.insert (hashUnique u) reg)

withExtRegFileTy :: Monad m => Reg -> Ty -> TalBuilderT m a -> TalBuilderT m a
withExtRegFileTy reg ty = locally regFileTy (rfRegTy %~ M.insert reg ty)

findReg :: (HasCallStack, MonadFail m) => Unique -> TalBuilderT m Reg
findReg u = do
    regs <- view regTable
    case IM.lookup (hashUnique u) regs of
        Just reg -> return reg
        Nothing  -> error $ "findReg: " ++ show (hashUnique u)

nextReg :: Monad m => TalBuilderT m Reg
nextReg = do
    regs <- use freeRegSet
    let reg = if null regs then error "no available registers" else S.findMin regs
    return reg

freshReg :: Monad m => TalBuilderT m Reg
freshReg = do
    reg <- nextReg
    setRegInUse reg
    return reg

setRegInUse :: Monad m => Reg -> TalBuilderT m ()
setRegInUse reg = freeRegSet %= S.delete reg

setRegFree :: Monad m => Reg -> TalBuilderT m ()
setRegFree reg = freeRegSet %= S.insert reg

getInUseRegs :: Monad m => TalBuilderT m [Reg]
getInUseRegs = do
    regs <- use freeRegSet
    return $ S.toList $ S.difference initialRegSet regs

isFreeReg :: Monad m => Reg -> TalBuilderT m Bool
isFreeReg reg = do
    regs <- use freeRegSet
    return $ S.member reg regs

-- ** Names

newName :: Monad m => Unique -> String -> TalBuilderT m Name
newName u hint = do
    let name = Name hint u
    nameTable %= IM.insert (hashUnique u) name
    return name

lookupName :: MonadFail m => Unique -> TalBuilderT m Name
lookupName u = do
    names <- use nameTable
    case IM.lookup (hashUnique u) names of
        Just name -> return name
        Nothing   -> fail $ "lookupName: " ++ show (hashUnique u)

-- * TyVars
withExtTyVarScope :: Monad m => Unique -> TalBuilderT m a -> TalBuilderT m a
withExtTyVarScope u = locally tyVarScope (u :)

getTyVar :: MonadFail m => Unique -> TalBuilderT m TyVar
getTyVar u = do
    sc <- view tyVarScope
    case L.elemIndex u sc of
        Just idx -> return idx
        Nothing  -> fail $ "getTyVar: " ++ show (hashUnique u)

-- ** UseCount

resetUseCount :: Monad m => Unique -> TalBuilderT m ()
resetUseCount u = useCount %= IM.insert (hashUnique u) 0

incUseCount :: Monad m => Unique -> TalBuilderT m ()
incUseCount u = useCount %= IM.update (Just . succ) (hashUnique u)

decUseCount :: Monad m => Unique -> TalBuilderT m ()
decUseCount u = useCount %= IM.update (Just . pred) (hashUnique u)

isUseCountZero :: Monad m => Unique -> TalBuilderT m Bool
isUseCountZero u = do
    cnt <- use useCount
    return $ (Just 0 ==) $ IM.lookup (hashUnique u) cnt

whenUseCountZero :: Monad m => Unique -> TalBuilderT m () -> TalBuilderT m ()
whenUseCountZero u act = do
    yes_zero <- isUseCountZero u
    when yes_zero act
