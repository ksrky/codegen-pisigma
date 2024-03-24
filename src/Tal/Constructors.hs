module Tal.Constructors (
    mkRegFileTy,
    mkArgumentRegs,
    (<>|),
    emptyHeaps,
    emptyRegFile,
    mkProgramFromInstrs
    ) where

import Data.Map.Strict qualified as M
import Tal.Constant
import Tal.Syntax

mkRegFileTy :: [Ty] -> RegFileTy
mkRegFileTy = M.fromList . zip argumentRegs

mkArgumentRegs :: Int -> [Reg]
mkArgumentRegs n
    | n <= numArgumentRegs = take n argumentRegs
    | otherwise = error "exceeded the number of argument registers"

infixr 5 <>|

(<>|) :: [Instr] -> Instrs -> Instrs
(<>|) inslist instrs = foldr ISeq instrs inslist

emptyHeaps :: Heaps
emptyHeaps = M.empty

emptyRegFile :: RegFile
emptyRegFile = M.empty

mkProgramFromInstrs :: Instrs -> Program
mkProgramFromInstrs instrs = (emptyHeaps, emptyRegFile, instrs)

