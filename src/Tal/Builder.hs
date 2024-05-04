{-# LANGUAGE TemplateHaskell #-}

module Tal.Builder where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap              qualified as IM
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Constant
import Tal.Syntax

data BuilderState = BuilderState
    { _instrState   :: Instrs
    , _heapsState   :: Heaps
    , _inUseRegSet  :: S.Set Reg
    , _inUseNameMap :: M.Map String Int
    , _nextUniq     :: Word
    , _nameTable    :: IM.IntMap Name
    }

makeClassy ''BuilderState

data BuilderContext = BuilderContext
    { _regTable  :: IM.IntMap Reg
    , _regFileTy :: RegFileTy
    }

makeClassy ''BuilderContext

type TalBuilder m = ReaderT BuilderContext (StateT BuilderState m)

extendHeaps :: Monad m => [(Name, Heap)] -> TalBuilder m ()
extendHeaps heaps = heapsState %= M.union (M.fromList heaps)

findReg :: Monad m => Int -> TalBuilder m Reg
findReg i = do
    regs <- view regTable
    case IM.lookup i regs of
        Just reg -> return reg
        Nothing  -> error $ "findReg: " ++ show i

withExtendReg :: Monad m => Int -> Reg -> TalBuilder m a -> TalBuilder m a
withExtendReg i reg = locally regTable (IM.insert i reg)

freshReg :: Monad m => TalBuilder m Reg
freshReg = do
    regs <- use inUseRegSet
    let reg = if null regs then Reg1 else S.findMin regs
    inUseRegSet %= S.insert reg
    return reg

freeReg :: Monad m => Reg -> TalBuilder m ()
freeReg reg = modifying inUseRegSet (S.delete reg)

freeAllRegs :: Monad m => TalBuilder m ()
freeAllRegs = inUseRegSet .= S.empty

newUniq :: Monad m => TalBuilder m Uniq
newUniq = do
    nextUniq %= (+ 1)
    use nextUniq

freshName :: Monad m => String -> TalBuilder m Name
freshName str = do
    names <- use inUseNameMap
    case M.lookup str names of
        Just num -> freshName $ str ++ "." ++ show num
        Nothing -> do
            inUseNameMap %= M.insert str 1
            nameTable %= IM.insert 1 (Name str 1)
            Name str <$> newUniq

lookupName :: Monad m => Int -> TalBuilder m Name
lookupName i = do
    names <- use nameTable
    case IM.lookup i names of
        Just name -> return name
        Nothing   -> error $ "lookupName: " ++ show i
