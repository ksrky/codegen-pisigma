module Tal.Context
    ( Context
    , createContext
    , getProgramEnd
    , getDataEnd
    ) where

import Foreign.C
import Foreign.Ptr

data Context

foreign import ccall unsafe "Context" createContext :: IO (Ptr Context)

foreign import ccall unsafe "getProgramEnd" getProgramEnd :: Ptr Context -> IO CInt

foreign import ccall unsafe "getDataEnd" getDataEnd :: Ptr Context -> IO CInt
