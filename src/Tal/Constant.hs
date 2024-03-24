module Tal.Constant (
    numArgumentRegs,
    argumentRegs,
    returnReg,
    sptrReg
    ) where

import Tal.Syntax

numArgumentRegs :: Int
numArgumentRegs = 4

argumentRegs :: [Reg]
argumentRegs = map GeneralReg [1 .. fromIntegral numArgumentRegs]

returnReg :: Reg
returnReg = SpecialReg "rv"

sptrReg :: Reg
sptrReg = SpecialReg "sp"
