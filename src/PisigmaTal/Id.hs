{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Id (
    Id(..),
    name,
    unique,
    idInt,
    newId,
    unsafeNewId,
    dummyId
) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Data.Unique
import GHC.IO.Unsafe
import Prettyprinter.Prec

data Id = Id {_name :: String, _unique :: Unique}

makeLenses ''Id

idInt :: Id -> Int
idInt x = hashUnique (x ^. unique)

instance Eq Id where
    x == y = x ^. unique == y ^. unique

instance Ord Id where
    compare x y = compare (x ^. name) (y ^. name)

instance Show Id where
    show = _name

instance PrettyPrec Id where
    pretty = pretty . _name

newId :: MonadIO m => String -> m Id
newId s = Id s <$> liftIO newUnique

unsafeNewId :: String -> Id
unsafeNewId s = unsafePerformIO $ newId s

dummyId :: Id
dummyId = unsafeNewId "dummy"
