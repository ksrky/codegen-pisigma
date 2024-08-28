module Tal.Interpreter
    ( RuntimeContext
    , createRuntimeContext
    , lookupRegister
    , lookupRV
    , step
    , steps
    , runInterpreter
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Tal.Context
import Tal.Instruction

data RuntimeContext

foreign import ccall unsafe "CreateRuntimeContext" createRuntimeContext :: Ptr Context -> IO (Ptr RuntimeContext)

foreign import ccall unsafe "LookupRegister" lookupRegister :: Ptr RuntimeContext -> CInt -> IO CInt

lookupRV :: Ptr RuntimeContext -> IO CInt
lookupRV ctx = lookupRegister ctx rv

foreign import ccall unsafe "Step" step :: Ptr RuntimeContext -> IO ()

foreign import ccall unsafe "Steps" steps :: Ptr RuntimeContext -> IO ()

runInterpreter :: Ptr Context -> IO Int
runInterpreter ctx = do
    rctx <- createRuntimeContext ctx
    steps rctx
    fromIntegral <$> lookupRV rctx
