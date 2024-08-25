module PisigmaTal.Tal.Constant
    ( initialRegSet
    , pattern Reg1
    , pattern A1Reg, pattern A2Reg, pattern A3Reg, pattern A4Reg
    , argumentRegs
    , pattern RVReg
    , pattern SPReg
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

pattern A1Reg, A2Reg, A3Reg, A4Reg :: Reg
pattern A1Reg = SpecialReg "a1"
pattern A2Reg = SpecialReg "a2"
pattern A3Reg = SpecialReg "a3"
pattern A4Reg = SpecialReg "a4"

argumentRegs :: [Reg]
argumentRegs = [A1Reg, A2Reg, A3Reg, A4Reg]

pattern RVReg :: Reg
pattern RVReg = SpecialReg "rv"

pattern SPReg :: Reg
pattern SPReg = SpecialReg "sp"
