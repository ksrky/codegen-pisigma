module Id (newUniq, Id(..), fromString) where

import Data.IORef
import GHC.IO             (unsafePerformIO)
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

data Id = Id {name :: String, uniq :: Uniq}
    deriving (Eq, Show)

fromString :: String -> Id
fromString s = Id s newUniq

instance PrettyPrec Id where
    pretty = pretty . name

