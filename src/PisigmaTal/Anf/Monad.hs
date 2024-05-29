{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Anf.Monad where

import Control.Lens.Combinators
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict          qualified as M
import PisigmaTal.Id

data AnfState = AnfState

data AnfContext = AnfContext
    { _anfIdCount    :: M.Map Id Int
    , _anfFunArities :: M.Map String Int
    }

instance Semigroup AnfContext where
    AnfContext a1 b1 <> AnfContext a2 b2 = AnfContext (a1 <> a2) (b1 <> b2)

makeLenses ''AnfContext

type AnfM m = ReaderT AnfContext (StateT AnfState m)

type AnfSetupM m = WriterT AnfContext m
