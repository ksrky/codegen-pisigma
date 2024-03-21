{-# LANGUAGE TemplateHaskell #-}

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
    Aop(..),
    Bop(..),
    Instr(..),
    Instrs(..),
    Program
) where

import Control.Lens.Cons
import Control.Lens.Prism
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map.Strict          qualified as M
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

instance Cons RowTy RowTy (Ty, InitFlag) (Ty, InitFlag) where
    _Cons = prism (uncurry RSeq) $ \case
        RSeq (ty, flag) row -> Right ((ty, flag), row)
        row                 -> Left row

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
    | HCode RegFileTy Instrs
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
    | ILoad Reg Reg Int
    | IMalloc Reg [Ty]
    | IMove Reg SmallVal
    | IStore Reg Int Reg
    | IUnpack Reg SmallVal
    -- | @salloc n@
    -- | ISalloc Int
    -- | @sfree n@
    -- | ISfree Int
    -- | @sload rd, sp(i)@
    -- | ISload Reg Int
    -- | @sstore sp(i), rs@
    -- | ISstore Int Reg
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

type Program = (Heaps, RegFile, Instrs)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy

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

class Subst a where
    subst :: Ty -> a -> a

instance Subst Ty where
    subst = substTop

instance Subst (Val a) where
    subst s = \case
        VReg r -> VReg r
        VWord v -> VWord v
        VLabel l -> VLabel l
        VInt i -> VInt i
        VJunk ty -> VJunk $ subst s ty
        VPack ty v ty' -> VPack (subst s ty) v (subst s ty')
        VRoll v ty -> VRoll v (subst s ty)
        VUnroll v -> VUnroll v
        VNonsense -> VNonsense

instance Subst Instr where
    subst s = \case
        IAop aop rd rs v -> IAop aop rd rs (subst s v)
        IBop bop rd v -> IBop bop rd (subst s v)
        ICall v -> ICall (subst s v)
        ILoad rd rs i -> ILoad rd rs i
        IMalloc rd tys -> IMalloc rd (map (subst s) tys)
        IMove rd v -> IMove rd (subst s v)
        IStore rd i rs -> IStore rd i rs
        IUnpack rd v -> IUnpack rd (subst s v)
        -- ISalloc n -> ISalloc n
        -- ISfree n -> ISfree n
        -- ISload rd i -> ISload rd i
        -- ISstore i rs -> ISstore i rs