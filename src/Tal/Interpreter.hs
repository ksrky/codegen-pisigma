module Tal.Interpreter
    ( RuntimeContext
    , createRuntimeContext
    , step
    , steps
    ) where

import Foreign.Ptr
import Tal.Context

data RuntimeContext

foreign import ccall unsafe "CreateRuntimeContext" createRuntimeContext :: Ptr Context -> IO (Ptr RuntimeContext)

foreign import ccall unsafe "step" step :: Ptr RuntimeContext -> IO ()

foreign import ccall unsafe "steps" steps :: Ptr RuntimeContext -> IO ()
