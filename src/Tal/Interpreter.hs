module Tal.Interpreter
    ( step
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Tal.Context
import Tal.Instruction

foreign import ccall unsafe "step" step :: Ptr Context -> Instruction -> IO ()
