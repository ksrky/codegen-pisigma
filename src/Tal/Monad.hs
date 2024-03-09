{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Tal.Monad where

import Control.Lens.Combinators
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Syntax

data TalContext = TalContext
    { _regEnv     :: [Reg]
    , _usedRegSet :: IORef (S.Set Reg)
    , _regFileTy  :: RegFileTy
    }

makeClassy ''TalContext

class MonadReader TalContext m => MonadTalCodegen m

instance Monad m => MonadTalCodegen (ReaderT TalContext m)

findReg :: MonadTalCodegen m => Int -> m Reg
findReg i = do
    regs <- view regEnv
    return $ regs !! i

withExtendReg :: MonadTalCodegen m => Reg -> m a -> m a
withExtendReg reg = locally regEnv (reg :)

withExtendRegs :: MonadTalCodegen m => [Reg] -> m a -> m a
withExtendRegs regs = locally regEnv (regs ++)

freshReg :: (HasTalContext r, MonadReader r m, MonadIO m) => m Reg
freshReg = do
    usedRegsRef <- view usedRegSet
    usedRegs <- liftIO $ readIORef usedRegsRef
    let reg = S.findMin usedRegs
    liftIO $ modifyIORef usedRegsRef (S.insert reg)
    return reg

freeReg :: (HasTalContext r, MonadReader r m, MonadIO m) => Reg -> m ()
freeReg reg = do
    usedRegsRef <- view usedRegSet
    liftIO $ modifyIORef usedRegsRef (S.delete reg)

freeRegSet :: (HasTalContext r, MonadReader r m, MonadIO m) => m ()
freeRegSet = do
    usedRegsRef <- view usedRegSet
    liftIO $ writeIORef usedRegsRef S.empty

data TalState = TalState
    { _heapsState  :: Heaps
    , _instrsState :: Instrs
    }

makeClassy ''TalState

type TalBuilderT m = StateT TalState (ReaderT TalContext m)

type TalBuilder = TalBuilderT IO

instance Monad m => MonadTalCodegen (TalBuilderT m)

initTalState :: TalState
initTalState = TalState
    { _heapsState = M.empty
    , _instrsState = IHalt TNonsense
    }

runTalBuilderT :: Monad m => TalBuilderT m a -> ReaderT TalContext m TalState
runTalBuilderT builder = snd <$> runStateT builder initTalState
