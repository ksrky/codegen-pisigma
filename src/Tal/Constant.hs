module Tal.Constant (
    numArgumentRegs,
    argumentRegs,
    initialRegSet,
    pattern RVReg,
    pattern SPReg,
    pattern Reg1,
    ) where

import Data.Set   qualified as S
import Tal.Syntax

-- * Registers

numArgumentRegs :: Int
numArgumentRegs = 4

argumentRegs :: [Reg]
argumentRegs = map GeneralReg [1 .. fromIntegral numArgumentRegs]

initialRegSet :: S.Set Reg
initialRegSet = S.fromList $ map GeneralReg [1 .. 16]

pattern RVReg :: Reg
pattern RVReg = SpecialReg "rv"

pattern SPReg :: Reg
pattern SPReg = SpecialReg "sp"

pattern Reg1 :: Reg
pattern Reg1 = GeneralReg 1
