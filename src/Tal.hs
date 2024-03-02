module Tal (
    Reg(..),
    retReg,
    Name(..),
    Label,
    TyVar,
    Ty(..),
    Row(..),
    HeapsTy,
    RegFileTy,
    mkRegFileTy,
    Telescopes,
    Val(..),
    WordVal,
    SmallVal,
    Heap(..),
    Heaps,
    RegFile,
    Instr(..),
    Instrs(..),
    Program
) where
import Data.Map.Strict qualified as M

newtype Reg = Reg {unReg :: Int}
    deriving (Eq, Ord, Show)

retReg :: Reg
retReg = Reg 0

newtype Name = Name String
    deriving (Eq, Show)

type Label = Name

type TyVar = Int

data Ty
    = TInt
    | TVar TyVar
    | TRegFile RegFileTy
    | TExists Ty
    | TRecurs Ty
    | TRow Row
    deriving (Eq, Show)

data Row = REmpty | RVar Int | (Ty, InitFlag) :> Row
    deriving (Eq, Show)

type InitFlag = Bool

type HeapsTy = [(Label, Ty)]

type RegFileTy = M.Map Reg Ty

mkRegFileTy :: [Ty] -> RegFileTy
mkRegFileTy = M.fromList . zip (map Reg [1..])

type Telescopes = [TyVar]

data NonReg

data Val a where
    VReg   :: Reg -> Val Reg
    VWord  :: Val NonReg -> Val Reg
    VLabel :: Label -> Val NonReg
    VInt   :: Int -> Val NonReg
    VJunk  :: Ty -> Val NonReg
    VPack  :: Ty -> Val a -> Ty -> Val a
    VRoll  :: Val a -> Ty -> Val a
    VUnroll :: Val a -> Val a

deriving instance Eq (Val a)
deriving instance Show (Val a)

type WordVal = Val NonReg

type SmallVal = Val Reg

data Heap
    = HGlobal WordVal
    | HCode RegFileTy Instrs
    deriving (Eq, Show)

type Heaps = [(Name, Heap)]

type RegFile = M.Map Reg WordVal

data Aop = Add | Sub | Mul
    deriving (Eq, Show)

data Bop = Bnz
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
    deriving (Eq, Show)

data Instrs
    = ISeq Instr Instrs
    | IJump SmallVal
    | IHalt Ty
    deriving (Eq, Show)

infixr 5 `ISeq`

type Program = (Heaps, RegFile, Instrs)
