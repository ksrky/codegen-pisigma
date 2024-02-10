{-# LANGUAGE TemplateHaskell #-}

module Id (
    newUniq,
    HasAttr(..),
    Id(..),
    name,
    attr,
    uniq,
    fromString) where

import Control.Lens.Combinators
import Data.IORef
import GHC.IO                   (unsafePerformIO)
import Prettyprinter.Prec

type Uniq = Int

uniqSupplier :: IO (IORef Uniq)
uniqSupplier = newIORef 0

-- | TODO: replace with a pure implementation
{-# NOINLINE newUniq #-}
newUniq :: Uniq
newUniq = unsafePerformIO $ do
    ref <- uniqSupplier
    uniq <- readIORef ref
    modifyIORef ref (+ 1)
    return uniq

newtype Attr = Attr {_extern :: Bool}
    deriving (Eq, Show)

class HasAttr a where
    attrL :: Lens' a Attr
    extern :: Lens' a Bool
    extern = attrL . go where go f (Attr x) = Attr <$> f x

instance HasAttr Attr where
    attrL = id

instance HasAttr a => HasAttr (a, b) where
    attrL = _1 . attrL

noAttr :: Attr
noAttr = Attr {_extern = False}

data Id = Id {_name :: String, _attr :: Attr, _uniq :: Uniq}
    deriving (Eq, Show)

makeLenses ''Id

fromString :: String -> Id
fromString s = Id s noAttr newUniq

instance HasAttr Id where
    attrL = lens _attr (\x y -> x {_attr = y})

instance PrettyPrec Id where
    pretty = pretty . _name
