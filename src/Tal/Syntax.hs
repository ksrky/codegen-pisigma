{-# LANGUAGE TemplateHaskell #-}

module Tal.Syntax (
    Reg(..),
    Uniq,
    Name(..),
    Label,
    TyVar,
    Ty(..),
    RowTy(..),
    StackTy(..),
    HeapsTy,
    RegFileTy,
    Telescopes,
    Ptr,
    Val(..),
    WordVal,
    SmallVal,
    Heap(..),
    Heaps,
    Stack,
    RegFile,
    Aop(..),
    Bop(..),
    Instr(..),
    Instrs(..),
    Program
) where

import Control.Lens.Cons
import Control.Lens.Prism
import Data.Functor.Foldable.TH
import Data.Map.Strict          qualified as M
import Data.Word

data Reg
    = GeneralReg Word8
    | SpecialReg String
    deriving (Eq, Ord, Show)

type Uniq = Word

data Name = Name
    { nameText :: String
    , nameUniq :: Uniq
    }
    deriving (Eq, Ord, Show)

type Label = Name

type TyVar = Int

data Ty
    = TInt
    | TVar TyVar
    | TRegFile RegFileTy
    | TExists Ty
    | TRecurs Ty
    | TRow RowTy
    | TNonsense
    | TPtr StackTy
    | TAlias Name
    deriving (Eq, Show)

data RowTy = REmpty | RVar Int | RSeq Ty RowTy
    deriving (Eq, Show)

instance Cons RowTy RowTy Ty Ty where
    _Cons = prism (uncurry RSeq) $ \case
        RSeq ty row -> Right (ty, row)
        row                 -> Left row

data StackTy = SNil | SVar Int | SCons Ty StackTy
    deriving (Eq, Show)

instance Cons StackTy StackTy Ty Ty where
    _Cons = prism (uncurry SCons) $ \case
        SCons ty stack -> Right (ty, stack)
        stack          -> Left stack

type HeapsTy = M.Map Label Ty

type RegFileTy = M.Map Reg Ty

type Telescopes = [TyVar]

data NonReg

-- | One-based index from the beginning of a stack.
type Ptr = Int

data Val a where
    VReg      :: Reg -> Val Reg
    VWord     :: Val NonReg -> Val Reg
    VLabel    :: Label -> Val NonReg
    VInt      :: Int -> Val NonReg
    VJunk     :: Ty -> Val NonReg
    VPack     :: Ty -> Val a -> Ty -> Val a
    VRoll     :: Val a -> Ty -> Val a
    VUnroll   :: Val a -> Val a
    VNonsense :: Val NonReg
    VPtr      :: Ptr -> Val NonReg

deriving instance Eq (Val a)
deriving instance Show (Val a)

type WordVal = Val NonReg

instance Num WordVal where
    VInt m + VInt n = VInt (m + n)
    _ + _           = error "Int required"
    VInt m - VInt n = VInt (m - n)
    _ - _           = error "Int required"
    VInt m * VInt n = VInt (m * n)
    _ * _           = error "Int required"
    abs (VInt n) = VInt (abs n)
    abs _        = error "Int required"
    signum (VInt n) = VInt (signum n)
    signum _        = error "Int required"
    fromInteger = VInt . fromInteger

type SmallVal = Val Reg

data Heap
    = HGlobal WordVal
    | HCode Telescopes RegFileTy Instrs
    | HStruct [WordVal]
    deriving (Eq, Show)

type Heaps = M.Map Name Heap

type RegFile = M.Map Reg WordVal

type Stack = [WordVal]

data Aop = Add | Sub | Mul
    deriving (Eq, Show)

data Bop = Bz | Bnz | Bgt | Blt
    deriving (Eq, Show)

data Instr
    = IAop Aop Reg Reg SmallVal
    | IBop Bop Reg SmallVal
    | ICall SmallVal
    -- | ICoerce _ Reg
    | ILoad Reg Reg Int
    | IMalloc Reg [Ty]
    | IMove Reg SmallVal
    | IStore Reg Int Reg
    | IUnpack Reg SmallVal
    -- | @salloc n@
    | ISalloc Int
    -- | @sfree n@
    | ISfree Int
    -- | @sload rd, rs(i)@
    | ISload Reg Reg Int
    -- | @sstore rd(i), rs@
    | ISstore Reg Int Reg
    deriving (Eq, Show)

data Instrs
    = ISeq Instr Instrs
    | IJump SmallVal
    | IHalt Ty
    deriving (Eq, Show)

infixr 5 `ISeq`

instance Cons Instrs Instrs Instr Instr where
    _Cons = prism (uncurry ISeq) $ \case
        ISeq i is -> Right (i, is)
        ins       -> Left ins

type Program = (Heaps, Instrs)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy

{-
mapTy :: (Int -> Int -> Ty) -> Int -> Ty -> Ty
mapTy onvar = flip $ cata $ \case
    TIntF -> const TInt
    TVarF x -> onvar x
    TRegFileF arg_tys -> \c -> TRegFile (M.map ($ c) arg_tys)
    TExistsF ty -> \c -> TExists (ty (c + 1))
    TRecursF ty -> \c -> TRecurs (ty (c + 1))
    TRowF row_ty -> \c -> TRow $ mapRowTy onvar c row_ty
    TNonsenseF -> const TNonsense

mapRowTy :: (Int -> Int -> Ty) -> Int -> RowTy -> RowTy
mapRowTy onvar c = cata $ \case
    REmptyF -> REmpty
    RVarF x -> case onvar x c of
        TRow row -> row
        TVar y   -> RVar y
        _        -> error "TRow or TVar required"
    RSeqF (ty, flag) row -> RSeq (mapTy onvar c ty, flag) row

shiftTy :: Int -> Ty -> Ty
shiftTy d = mapTy (\x c -> TVar (if x < c then x else x + d)) 0

substTy :: Ty -> Ty -> Ty
substTy s = mapTy (\x j -> if x == j then shiftTy j s else TVar x) 0

substTop :: Ty -> Ty -> Ty
substTop s t = shiftTy (-1) (substTy (shiftTy 1 s) t)
-}
