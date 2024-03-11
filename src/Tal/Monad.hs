{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Tal.Monad where

import Control.Lens.Combinators
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Syntax

data TalContext = TalContext
    { _regEnv      :: [Reg]
    , _usedRegSet  :: IORef (S.Set Reg)
    , _regFileTy   :: RegFileTy
    , _usedNameMap :: IORef (M.Map String Int)
    }

makeClassy ''TalContext

class MonadReader TalContext m => MonadTalBuilder m

instance Monad m => MonadTalBuilder (ReaderT TalContext m)

runTalBuilder :: MonadIO m => ReaderT TalContext m a -> m a
runTalBuilder builder = do
    _usedRegSet <- liftIO $ newIORef S.empty
    _usedNameMap <- liftIO $ newIORef M.empty
    runReaderT builder $
        TalContext
            { _regEnv = []
            , _usedRegSet
            , _regFileTy = M.empty
            , _usedNameMap
            }

findReg :: MonadTalBuilder m => Int -> m Reg
findReg i = do
    regs <- view regEnv
    return $ regs !! i

withExtendReg :: MonadTalBuilder m => Reg -> m a -> m a
withExtendReg reg = locally regEnv (reg :)

withExtendRegs :: MonadTalBuilder m => [Reg] -> m a -> m a
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

freshName :: (HasTalContext r, MonadReader r m, MonadIO m) => String -> m Name
freshName str = do
    usedNamesRef <- view usedNameMap
    usedNames <- liftIO $ readIORef usedNamesRef
    case M.lookup str usedNames of
        Just num -> freshName $ str ++ "." ++ show num
        Nothing -> do
            liftIO $ writeIORef usedNamesRef (M.insert str 1 usedNames)
            return $ Name str
