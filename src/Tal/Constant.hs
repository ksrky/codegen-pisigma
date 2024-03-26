module Tal.Constant (
    numArgumentRegs,
    argumentRegs,
    pattern RVReg,
    pattern SPReg,
    reg1, reg2, reg3, reg4,
    ) where

import Tal.Syntax

-- * Registers

numArgumentRegs :: Int
numArgumentRegs = 4

argumentRegs :: [Reg]
argumentRegs = map GeneralReg [1 .. fromIntegral numArgumentRegs]

pattern RVReg :: Reg
pattern RVReg = SpecialReg "rv"

pattern SPReg :: Reg
pattern SPReg = SpecialReg "sp"

reg1, reg2, reg3, reg4 :: Reg
reg1 = GeneralReg 1
reg2 = GeneralReg 2
reg3 = GeneralReg 3
reg4 = GeneralReg 4
