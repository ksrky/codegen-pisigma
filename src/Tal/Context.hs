module Tal.Context
    ( Context
    , createTalContext
    ) where

import Foreign.Ptr

data Context

foreign import ccall unsafe "Context" createTalContext :: IO (Ptr Context)
