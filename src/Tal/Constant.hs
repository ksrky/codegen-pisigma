module Tal.Constant where

import Data.Word
import Tal.Syntax

numArgumentRegs :: Int
numArgumentRegs = 4

argumentRegs :: [Reg]
argumentRegs = map GeneralReg [1 .. fromIntegral numArgumentRegs]

returnReg :: Reg
returnReg = GeneralReg 1
