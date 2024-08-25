module Tal.Interpreter
    ( RuntimeContext
    , step
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Tal.Instruction

data RuntimeContext

foreign import ccall unsafe "step" step :: Ptr RuntimeContext -> Instruction -> IO ()
