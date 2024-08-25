module PisigmaTal.Tal.Constant
    ( initialRegSet
    , pattern Reg1
    , pattern A0Reg, pattern A1Reg, pattern A2Reg, pattern A3Reg
    , argumentRegs
    , pattern ZRReg, pattern RAReg, pattern SPReg, pattern RVReg
    ) where

import Data.Set       qualified as S
import PisigmaTal.Tal

-- * Registers
-- ** General registers

pattern Reg1 :: Reg
pattern Reg1 = GeneralReg 1

initialRegSet :: S.Set Reg
initialRegSet = S.fromList $ map GeneralReg [1 .. 16]

-- **Special regsiters

pattern A0Reg, A1Reg, A2Reg, A3Reg :: Reg
pattern A0Reg = SpecialReg "a0"
pattern A1Reg = SpecialReg "a1"
pattern A2Reg = SpecialReg "a2"
pattern A3Reg = SpecialReg "a3"

argumentRegs :: [Reg]
argumentRegs = [A0Reg, A1Reg, A2Reg, A3Reg]

pattern ZRReg, RAReg, SPReg, RVReg :: Reg
pattern ZRReg = SpecialReg "zr"
pattern RAReg = SpecialReg "ra"
pattern SPReg = SpecialReg "sp"
pattern RVReg = SpecialReg "rv"
