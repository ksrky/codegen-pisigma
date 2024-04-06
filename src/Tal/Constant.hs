module Tal.Constant (
    numArgumentRegs,
    argumentRegs,
    pattern RVReg,
    pattern SPReg,
    pattern Reg1,
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

pattern Reg1 :: Reg
pattern Reg1 = GeneralReg 1
