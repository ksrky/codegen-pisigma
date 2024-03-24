{-# LANGUAGE TemplateHaskell #-}

module Tal.State where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Tal.Syntax

data TalState = TalState
    { _heaps    :: Heaps
    , _regFile  :: RegFile
    , _nextUniq :: Word
    }

makeClassy ''TalState

class MonadState TalState m => MonadTalState m

instance Monad m => MonadTalState (StateT TalState m)

evalTalState :: Monad m => StateT TalState m a -> TalState -> m a
evalTalState = evalStateT

defaultTalState :: TalState
defaultTalState = TalState
    { _heaps = M.empty
    , _regFile = M.empty
    , _nextUniq = 0
    }

extendHeap  :: MonadTalState m => (Name, Heap) -> m ()
extendHeap (name, heap) = heaps %= M.insert name heap

lookupHeap :: MonadTalState m => Name -> m (Maybe Heap)
lookupHeap name = M.lookup name <$> use heaps

lookupRegFile :: MonadTalState m => Reg -> m (Maybe WordVal)
lookupRegFile reg = M.lookup reg <$> use regFile

getHeap :: (MonadTalState m, MonadFail m) => Name -> m Heap
getHeap name = lookupHeap name >>= \case
    Just h -> return h
    Nothing -> fail "heap not found"

freshName :: MonadTalState m => String -> m Name
freshName str = do
    uniq <- nextUniq <<%= (+ 1)
    return $ Name str uniq

extendRegFile :: MonadTalState m => Reg -> WordVal -> m ()
extendRegFile reg val = regFile %= M.insert reg val

readReg :: (MonadTalState m, MonadFail m) => Reg -> m WordVal
readReg reg = lookupRegFile reg >>= \case
    Just w -> return w
    Nothing -> fail "register not found"

wordize :: (MonadTalState m, MonadFail m) => SmallVal -> m WordVal
wordize (VReg r)         = readReg r
wordize (VWord w)        = return w
wordize (VPack ty v ty') = VPack ty <$> wordize v <*> pure ty'
wordize (VRoll v ty)     = VRoll <$> wordize v <*> pure ty
wordize (VUnroll v)      = VUnroll <$> wordize v
