module Tal.Constructors (
    mkAbstractStackTy,
    mkArgumentRegs,
    (<>|),
    emptyHeaps,
    emptyRegFile,
    emptyRegFileTy,
    mkProgramFromInstrs
    ) where

import Control.Lens.Cons
import Data.Map.Strict   qualified as M
import Tal.Constant
import Tal.Syntax

mkAbstractStackTy :: TyVar -> [Ty] -> StackTy
mkAbstractStackTy rho = foldr SCons (SVar rho)

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

emptyRegFileTy :: RegFileTy
emptyRegFileTy = RegFileTy { _rfRegTy = M.empty, _rfStackTy = Nothing }

mkProgramFromInstrs :: Instrs -> Program
mkProgramFromInstrs instrs = (emptyHeaps, instrs)
 