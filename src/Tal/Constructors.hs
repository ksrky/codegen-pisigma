module Tal.Constructors
    ( emptyHeaps
    , emptyRegFile
    , emptyRegFileTy
    , mkArgRegFileTy
    , mkAbstractStackTy
    , mkProgramFromInstrs
    , (<>|)
    ) where

import Control.Lens.Cons
import Control.Lens.Operators
import Data.Map.Strict        qualified as M
import Tal.Constant
import Tal.Syntax

emptyHeaps :: Heaps
emptyHeaps = M.empty

emptyRegFile :: RegFile
emptyRegFile = M.empty

emptyRegFileTy :: RegFileTy
emptyRegFileTy = RegFileTy { _rfRegTy = M.empty, _rfStackTy = Nothing }

mkArgRegFileTy :: [Ty] -> RegFileTy
mkArgRegFileTy tys
    | length tys > length argumentRegs = error "not allowed" -- TODO: splilling
    | otherwise = emptyRegFileTy & rfRegTy .~ M.fromList (zip argumentRegs tys)

mkAbstractStackTy :: TyVar -> [Ty] -> StackTy
mkAbstractStackTy rho = foldr SCons (SVar rho)

mkProgramFromInstrs :: Instrs -> Program
mkProgramFromInstrs instrs = (emptyHeaps, instrs)

infixr 5 <>|

(<>|) :: (Foldable t, Cons a a b b) => t b -> a -> a
(<>|) bs a = foldr (<|) a bs
