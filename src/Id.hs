{-# LANGUAGE TemplateHaskell #-}

module Id (
    newUniq,
    HasAttr(..),
    Id(..),
    name,
    uniq,
    newId,
    newIdUnsafe
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

newtype Attr = Attr {_extern :: Bool}
    deriving (Eq, Show)

makeClassy ''Attr

instance HasAttr a => HasAttr (a, b) where
    attr = _1 . attr

noAttr :: Attr
noAttr = Attr {_extern = False}

data Id = Id {_name :: String, _attr :: Attr, _uniq :: Uniq}

makeLensesFor [("_name", "name"), ("_uniq", "uniq")] ''Id

instance Eq Id where
    x == y = x ^. uniq == y ^. uniq

instance Show Id where
    show = _name

instance HasAttr Id where
    attr = lens _attr (\x y -> x {_attr = y})

instance PrettyPrec Id where
    pretty = pretty . _name

newId :: MonadIO m => String -> m Id
newId s = Id s noAttr <$> newUniq

newIdUnsafe :: String -> Id
newIdUnsafe s = unsafePerformIO $ newId s
