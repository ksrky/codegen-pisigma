{-# LANGUAGE TemplateHaskell #-}

module Tal.Builder where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap              qualified as IM
import Data.List                qualified as L
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Constant
import Tal.Syntax

data BuilderState = BuilderState
    { _instrStack   :: [Instr]
    , _heapsState   :: Heaps
    , _freeRegSet   :: S.Set Reg
    , _inUseNameMap :: M.Map String Int
    , _nextUniq     :: Word
    , _nameTable    :: IM.IntMap Name
    , _useCount     :: IM.IntMap Int
    }

makeClassy ''BuilderState

data BuilderContext = BuilderContext
    { _regTable   :: IM.IntMap Reg
    , _regFileTy  :: RegFileTy
    , _tyVarScope :: [Int]
    , _quants     :: Quants
    }

makeClassy ''BuilderContext

type TalBuilderT m = ReaderT BuilderContext (StateT BuilderState m)

runTalBuilder :: Monad m => BuilderContext -> BuilderState -> TalBuilderT m a -> m a
runTalBuilder c s b = evalStateT (runReaderT b c) s

initBuilderContext :: BuilderContext
initBuilderContext = BuilderContext
    { _regTable   = IM.empty
    , _regFileTy  = M.empty
    , _tyVarScope = []
    , _quants     = []
    }

initBuilderState :: BuilderState
initBuilderState = BuilderState
    { _instrStack   = []
    , _heapsState   = M.empty
    , _freeRegSet   = initialRegSet
    , _inUseNameMap = M.empty
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

{-# INLINE extInstrs #-}
extInstrs :: Monad m => [Instr] -> TalBuilderT m ()
extInstrs instrs = instrStack %= (\l -> foldl (flip (:)) l instrs)

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
    if yes_free then do
        setRegInUse reg
        return reg
    else buildMove' (VReg reg)
buildMove val = buildMove' val

buildMove' :: Monad m => SmallVal -> TalBuilderT m Reg
buildMove' val = do
    reg <- freshReg
    extInstr $ IMove reg val
    return reg

buildPush :: MonadFail m => Reg -> TalBuilderT m ()
buildPush reg = extInstr $ IStore SPReg 0 reg

buildPop :: Monad m => Reg -> TalBuilderT m ()
buildPop reg = extInstr $ ILoad reg SPReg 0

-- ** Regs

findReg :: MonadFail m => Int -> TalBuilderT m Reg
findReg i = do
    regs <- view regTable
    case IM.lookup i regs of
        Just reg -> return reg
        Nothing  -> error $ "findReg: " ++ show i

withExtRegTable :: Monad m => Int -> Reg -> TalBuilderT m a -> TalBuilderT m a
withExtRegTable i reg = locally regTable (IM.insert i reg)

withExtRegFileTy :: Monad m => Reg -> Ty -> TalBuilderT m a -> TalBuilderT m a
withExtRegFileTy reg ty = locally regFileTy (M.insert reg ty)

setRegInUse :: Monad m => Reg -> TalBuilderT m ()
setRegInUse reg = freeRegSet %= S.delete reg

getInUseRegs :: Monad m => TalBuilderT m [Reg]
getInUseRegs = do
    regs <- use freeRegSet
    return $ S.toList $ S.difference initialRegSet regs

freshReg :: Monad m => TalBuilderT m Reg
freshReg = do
    regs <- use freeRegSet
    let reg = if null regs then error "no available registers" else S.findMin regs
    setRegInUse reg
    return reg

setRegFree :: Monad m => Reg -> TalBuilderT m ()
setRegFree reg = freeRegSet %= S.insert reg

isFreeReg :: Monad m => Reg -> TalBuilderT m Bool
isFreeReg reg = do
    regs <- use freeRegSet
    return $ S.member reg regs

newUniq :: Monad m => TalBuilderT m Uniq
newUniq = do
    nextUniq %= (+ 1)
    use nextUniq

-- ** Names

freshName :: Monad m => String -> TalBuilderT m Name
freshName str = do
    names <- use inUseNameMap
    case M.lookup str names of
        Just num -> freshName $ str ++ "." ++ show num
        Nothing -> do
            inUseNameMap %= M.insert str 1
            nameTable %= IM.insert 1 (Name str 1)
            Name str <$> newUniq

lookupName :: MonadFail m => Int -> TalBuilderT m Name
lookupName i = do
    names <- use nameTable
    case IM.lookup i names of
        Just name -> return name
        Nothing   -> fail $ "lookupName: " ++ show i

-- * TyVars
withExtTyVarScope :: Monad m => Int -> TalBuilderT m a -> TalBuilderT m a
withExtTyVarScope i = locally tyVarScope (i :)

getTyVar :: MonadFail m => Int -> TalBuilderT m TyVar
getTyVar i = do
    sc <- view tyVarScope
    case L.elemIndex i sc of
        Just idx -> return idx
        Nothing  -> fail $ "getTyVar: " ++ show i

-- ** UseCount

decUseCount :: Monad m => Int -> TalBuilderT m ()
decUseCount i = useCount %= IM.update (Just . pred) i

isUseCountZero :: Monad m => Int -> TalBuilderT m Bool
isUseCountZero i = do
    cnt <- use useCount
    return $ (Just 0 ==) $ IM.lookup i cnt

whenUseCountZero :: Monad m => Int -> TalBuilderT m () -> TalBuilderT m ()
whenUseCountZero i act = do
    zero <- isUseCountZero i
    when zero act
