module Tal.Syntax (
    Reg(..),
    Name(..),
    Label,
    TyVar,
    Ty(..),
    RowTy(..),
    StackTy(..),
    HeapsTy,
    RegFileTy,
    Telescopes,
    Val(..),
    WordVal,
    SmallVal,
    Heap(..),
    Heaps,
    Stack,
    RegFile,
    Instr(..),
    Instrs(..),
    Program
) where

import Data.Map.Strict qualified as M
import Data.Word

data Reg
    = GeneralReg Word8
    | SpecialReg String
    deriving (Eq, Ord, Show)

newtype Name = Name String
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
    deriving (Eq, Show)

type InitFlag = Bool

data RowTy = REmpty | RVar Int | RSeq (Ty, InitFlag) RowTy
    deriving (Eq, Show)

data StackTy = SNil | SVar Int | SCons Ty StackTy
    deriving (Eq, Show)

type HeapsTy = M.Map Label Ty

type RegFileTy = M.Map Reg Ty

type Telescopes = [TyVar]

data NonReg

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

deriving instance Eq (Val a)
deriving instance Show (Val a)

type WordVal = Val NonReg

type SmallVal = Val Reg

data Heap
    = HGlobal WordVal
    | HCode RegFileTy Instrs
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
    | ICall Reg SmallVal [Reg]
    | ILoad Reg Reg Int
    | IMalloc Reg [Ty]
    | IMove Reg SmallVal
    | IStore Reg Int Reg
    | IUnpack Reg SmallVal
    -- | @salloc n@
    | ISalloc Int
    -- | @sfree n@
    | ISfree Int
    -- | @sload rd, sp(i)@
    | ISload Reg Int
    -- | @sstore sp(i), rs@
    | ISstore Int Reg
    deriving (Eq, Show)

data Instrs
    = ISeq Instr Instrs
    | IJump SmallVal
    | IHalt Ty
    deriving (Eq, Show)

infixr 5 `ISeq`

type Program = (Heaps, RegFile, Instrs)
