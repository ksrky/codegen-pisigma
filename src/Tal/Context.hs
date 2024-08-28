module Tal.Context
    ( Context
    , createContext
    , getProgramAddress
    , getDataAddress
    ) where

import Foreign.C
import Foreign.Ptr

data Context

foreign import ccall unsafe "CreateTalContext" createContext :: IO (Ptr Context)

foreign import ccall unsafe "GetProgramAddress" getProgramAddress :: Ptr Context -> IO CInt

foreign import ccall unsafe "GetDataAddress" getDataAddress :: Ptr Context -> IO CInt
