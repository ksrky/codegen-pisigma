module Tal.Constructors (
    mkRegFileTy,
    mkArgumentRegs,
    (<>|),
    emptyHeaps,
    emptyRegFile,
    mkProgramFromInstrs
    ) where

import Control.Lens.Cons
import Data.Map.Strict   qualified as M
import Tal.Constant
import Tal.Syntax

mkRegFileTy :: [Ty] -> RegFileTy
mkRegFileTy = M.fromList . zip argumentRegs

mkArgumentRegs :: Int -> [Reg]
mkArgumentRegs n
    | n <= numArgumentRegs = take n argumentRegs
    | otherwise = error "exceeded the number of argument registers"

infixr 5 <>|

(<>|) :: (Foldable t, Cons a a b b) => t b -> a -> a
(<>|) bs a = foldr (<|) a bs

emptyHeaps :: Heaps
emptyHeaps = M.empty

emptyRegFile :: RegFile
emptyRegFile = M.empty

mkProgramFromInstrs :: Instrs -> Program
mkProgramFromInstrs instrs = (emptyHeaps, emptyRegFile, instrs)

