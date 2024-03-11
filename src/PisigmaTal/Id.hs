{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Id (
    newUniq,
    Id(..),
    name,
    uniq,
    newId,
    newIdUnsafe,
    dummyId
) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Data.IORef
import GHC.IO.Unsafe
import Prettyprinter.Prec

type Uniq = IORef ()

newUniq :: MonadIO m => m Uniq
newUniq = liftIO $ newIORef ()

data Id = Id {_name :: String, _uniq :: Uniq}

makeLensesFor [("_name", "name"), ("_uniq", "uniq")] ''Id

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

newIdUnsafe :: String -> Id
newIdUnsafe s = unsafePerformIO $ newId s

dummyId :: Id
dummyId = newIdUnsafe "dummy"
