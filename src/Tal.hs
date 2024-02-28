module Tal (
    Reg(..),
    Name(..),
    Label(..),
    TyVar,
    Ty(..),
    Row(..),
    HeapsTy,
    RegFileTy,
    WordVal(..),
    Val(..),
    Heap(..),
    Heaps,
    RegFile,
    Instr(..),
    Instrs(..),
    Program
) where

newtype Reg = Reg Int
    deriving (Eq, Show)

newtype Name = Name String
    deriving (Eq, Show)

newtype Label = Label String
    deriving (Eq, Show)

type TyVar = Int

data Ty
    = TInt
    | TVar TyVar
    | TRegFile RegFileTy
    | TExists Ty
    | TRecurs Ty
    | TRow Row
    deriving (Eq, Show)

data Row = REmpty | RVar Int | Ty :> Row
    deriving (Eq, Show)

type HeapsTy = [(Label, Ty)]

type RegFileTy = [(Reg, Int)]

data WordVal
    = WLabel Label
    | WInt Int
    | WJunk Ty
    | WPack Ty WordVal Ty
    deriving (Eq, Show)

data Val
    = VReg Reg
    | VWord WordVal
    | VPack Ty Val Ty
    deriving (Eq, Show)

data Heap
    = HRow [WordVal]
    | WCode RegFile Instrs
    deriving (Eq, Show)

type Heaps = [(Name, Heap)]

type RegFile = [(Reg, WordVal)]

data Instr
    = IAdd Reg Reg Val
    | IBnz Reg Val
    | ILoad Reg Reg Int
    | IMalloc Reg [Ty]
    | IMove Reg Val
    | IMul Reg Reg Val
    | IStore Reg Int Reg
    | ISub Reg Reg Val
    | IUnpack TyVar Reg Val
    deriving (Eq, Show)

data Instrs
    = ISeq Instr Instrs
    | IJmp Val
    | IRet Val
    deriving (Eq, Show)

type Program = (Heaps, RegFile, Instrs)
