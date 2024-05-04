{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Id (
    newUniq,
    Id(..),
    name,
    uniq,
    newId,
    unsafeNewId,
    dummyId
) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Data.IORef
import GHC.IO.Unsafe
import Prettyprinter.Prec

newtype Uniq = Uniq Int
    deriving (Eq, Ord, Show, Num)

{-# NOINLINE uniqSupply #-}
uniqSupply :: IORef Uniq
uniqSupply = unsafePerformIO $ newIORef 0

newUniq :: MonadIO m => m Uniq
newUniq = liftIO $! do
    modifyIORef uniqSupply (+ 1)
    readIORef uniqSupply

data Id = Id {_name :: String, _uniq :: Uniq}

makeLenses ''Id

instance Eq Id where
    x == y = x ^. uniq == y ^. uniq

instance Ord Id where
    compare x y = compare (x ^. name) (y ^. name)

instance Show Id where
    show = _name

instance PrettyPrec Id where
    pretty = pretty . _name

newId :: MonadIO m => String -> m Id
newId s = Id s <$> newUniq

unsafeNewId :: String -> Id
unsafeNewId s = unsafePerformIO $ newId s

dummyId :: Id
dummyId = unsafeNewId "dummy"
