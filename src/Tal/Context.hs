{-# LANGUAGE TemplateHaskell #-}

module Tal.Context (
    HasTalContext(..),
    MonadTalBuilder,
    runTalBuilder,
    findReg,
    withExtendReg,
    withExtendRegs,
    withExtendRegTy,
    freshReg,
    freeReg,
    freeAllRegs,
    freeRegSet,
    isInUseReg,
    freshName,
    ) where

import Control.Lens.Combinators
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict          qualified as M
import Data.Set                 qualified as S
import Tal.Constant
import Tal.Syntax

data TalContext = TalContext
    { _regEnv       :: [Reg]
    , _regFileTy    :: RegFileTy
    , _inUseRegSet  :: IORef (S.Set Reg)
    , _nextUniq     :: IORef Word
    , _inUseNameMap :: IORef (M.Map String Int)
    }

makeClassy ''TalContext

class MonadReader TalContext m => MonadTalBuilder m

instance Monad m => MonadTalBuilder (ReaderT TalContext m)

runTalBuilder :: MonadIO m => ReaderT TalContext m a -> m a
runTalBuilder builder = do
    _inUseRegSet <- liftIO $ newIORef S.empty
    _nextUniq <- liftIO $ newIORef 0
    _inUseNameMap <- liftIO $ newIORef M.empty
    runReaderT builder $
        TalContext
            { _regEnv = []
            , _inUseRegSet
            , _regFileTy = M.empty
            , _nextUniq
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
    ref <- view inUseRegSet
    regs <- liftIO $ readIORef ref
    let reg = if null regs then Reg1 else S.findMin regs
    liftIO $ modifyIORef ref (S.insert reg)
    return reg

freeReg :: (HasTalContext r, MonadReader r m, MonadIO m) => Reg -> m ()
freeReg reg = do
    ref <- view inUseRegSet
    liftIO $ modifyIORef ref (S.delete reg)

freeAllRegs :: (HasTalContext r, MonadReader r m, MonadIO m) => m ()
freeAllRegs = do
    ref <- view inUseRegSet
    liftIO $ writeIORef ref S.empty

freeRegSet :: (HasTalContext r, MonadReader r m, MonadIO m) => m ()
freeRegSet = do
    ref <- view inUseRegSet
    liftIO $ writeIORef ref S.empty

isInUseReg :: (HasTalContext r, MonadReader r m, MonadIO m) => Reg -> m Bool
isInUseReg reg = do
    ref <- view inUseRegSet
    regs <- liftIO $ readIORef ref
    return $ S.member reg regs

newUniq :: (HasTalContext r, MonadReader r m, MonadIO m) => m Uniq
newUniq = do
    ref <- view nextUniq
    liftIO $ modifyIORef ref (+ 1)
    liftIO $ readIORef ref

freshName :: (HasTalContext r, MonadReader r m, MonadIO m) => String -> m Name
freshName str = do
    ref <- view inUseNameMap
    names <- liftIO $ readIORef ref
    case M.lookup str names of
        Just num -> freshName $ str ++ "." ++ show num
        Nothing -> do
            liftIO $ writeIORef ref (M.insert str 1 names)
            Name str <$> newUniq
