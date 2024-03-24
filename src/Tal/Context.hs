{-# LANGUAGE TemplateHaskell #-}

module Tal.Context where

import Control.Lens.Combinators
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Syntax

data TalContext = TalContext
    { _regEnv       :: [Reg]
    , _regFileTy    :: RegFileTy
    , _inUseRegSet  :: IORef (S.Set Reg)
    , _inUseNameMap :: IORef (M.Map String Int)
    }

makeClassy ''TalContext

class MonadReader TalContext m => MonadTalBuilder m

instance Monad m => MonadTalBuilder (ReaderT TalContext m)

runTalBuilder :: MonadIO m => ReaderT TalContext m a -> m a
runTalBuilder builder = do
    _inUseRegSet <- liftIO $ newIORef S.empty
    _inUseNameMap <- liftIO $ newIORef M.empty
    runReaderT builder $
        TalContext
            { _regEnv = []
            , _inUseRegSet
            , _regFileTy = M.empty
            , _inUseNameMap
            }

findReg :: MonadTalBuilder m => Int -> m Reg
findReg i = do
    regs <- view regEnv
    return $ regs !! i

withExtendReg :: MonadTalBuilder m => Reg -> m a -> m a
withExtendReg reg = locally regEnv (reg :)

withExtendRegs :: MonadTalBuilder m => [Reg] -> m a -> m a
withExtendRegs regs = locally regEnv (regs ++)

withExtendRegTy :: MonadTalBuilder m => Reg -> Ty -> m a -> m a
withExtendRegTy reg ty = locally regFileTy (M.insert reg ty)

freshReg :: (HasTalContext r, MonadReader r m, MonadIO m) => m Reg
freshReg = do
    usedRegsRef <- view inUseRegSet
    usedRegs <- liftIO $ readIORef usedRegsRef
    let reg = S.findMin usedRegs
    liftIO $ modifyIORef usedRegsRef (S.insert reg)
    return reg

freeReg :: (HasTalContext r, MonadReader r m, MonadIO m) => Reg -> m ()
freeReg reg = do
    usedRegsRef <- view inUseRegSet
    liftIO $ modifyIORef usedRegsRef (S.delete reg)

freeRegSet :: (HasTalContext r, MonadReader r m, MonadIO m) => m ()
freeRegSet = do
    usedRegsRef <- view inUseRegSet
    liftIO $ writeIORef usedRegsRef S.empty

freshName :: (HasTalContext r, MonadReader r m, MonadIO m) => String -> m Name
freshName str = do
    ref <- view inUseNameMap
    names <- liftIO $ readIORef ref
    case M.lookup str names of
        Just num -> freshName $ str ++ "." ++ show num
        Nothing -> do
            liftIO $ writeIORef ref (M.insert str 1 names)
            return $ Name str
