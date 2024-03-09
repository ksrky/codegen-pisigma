module Tal.Constructors where

import Data.Map.Strict qualified as M
import Tal.Constant
import Tal.Syntax

mkRegFileTy :: [Ty] -> RegFileTy
mkRegFileTy = M.fromList . zip argumentRegs

mkArgumentRegs :: Int -> [Reg]
mkArgumentRegs n
    | n <= numArgumentRegs = take n argumentRegs
    | otherwise = error "exceeded the number of argument registers"

