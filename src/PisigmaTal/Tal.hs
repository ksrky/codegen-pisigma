{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Tal (
    Reg(..),
    Name(..),
    Label,
    TyVar,
    Ty(..),
    RowTy(..),
    StackTy(..),
    HeapsTy,
    RegFileTy(..),
    rfRegTy, rfStackTy,
    Quants,
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
    Program,
    substTop,
) where

import Control.Lens.At
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Functor.Foldable.TH
import Data.Map.Strict          qualified as M
import Data.Unique
import Data.Word

data Reg
    = GeneralReg Word8
    | SpecialReg String
    deriving (Eq, Ord, Show)

data Name = Name
    { nameText :: String
    , nameUniq :: Unique
    }
    deriving (Eq, Ord)

instance Show Name where
    show = nameText

type Label = Name

type TyVar = Int

type Quants = [()]

data Ty
    = TInt
    | TVar TyVar
    | TRegFile Quants RegFileTy
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

type instance Index RowTy = Int

type instance IxValue RowTy = Ty
instance Ixed RowTy where
    ix _ _ REmpty        = pure REmpty
    ix _ _ (RVar x)      = pure $ RVar x
    ix 0 f (RSeq ty row) = f ty <&> (`RSeq` row)
    ix k f (RSeq ty row) | k < 0     = error "negative index"
                         | otherwise = RSeq ty <$> ix (k - 1) f row

data StackTy
    = SNil
    | SVar Int
    | SCons Ty StackTy
    | SComp StackTy StackTy
    deriving (Eq, Show)

instance Cons StackTy StackTy Ty Ty where
    _Cons = prism (uncurry SCons) $ \case
        SCons ty stack -> Right (ty, stack)
        stack          -> Left stack

type instance Index StackTy = Int

type instance IxValue StackTy = Ty
instance Ixed StackTy where
    ix _ _ SNil          = pure SNil
    ix _ _ (SVar x)      = pure $ SVar x
    ix 0 f (SCons ty s) = f ty <&> (`SCons` s)
    ix k f (SCons ty s) | k < 0     = error "negative index"
                        | otherwise = SCons ty <$> ix (k - 1) f s
    ix _ _ SComp{} = error "composition of stack"

type HeapsTy = M.Map Label Ty

data RegFileTy = RegFileTy
    { _rfRegTy   :: M.Map Reg Ty
    , _rfStackTy :: Maybe StackTy
    }
    deriving (Eq, Show)

makeLenses ''RegFileTy

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

type SmallVal = Val Reg

data Heap
    = HGlobal WordVal
    | HCode Quants RegFileTy Instrs
    | HStruct [WordVal]
    | HExtern Ty
    | HTypeAlias Ty
    deriving (Eq, Show)

type Heaps = M.Map Name Heap

type RegFile = M.Map Reg WordVal

type Stack = [WordVal]

data Aop = Add | Sub | Mul | Div
    deriving (Eq, Show)

data Bop = Bz | Bnz | Bgt | Blt
    deriving (Eq, Show)

data Instr
    = IAop Aop Reg Reg SmallVal
    | IBop Bop Reg SmallVal
    | ICall Ty SmallVal
    -- | ICoerce _ Reg
    | ILoad Reg Reg Int
    | IMalloc Reg [Ty]
    | IMove Reg SmallVal
    | IStore Reg Int Reg
    | IUnpack Reg
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

mapTy :: (Int -> Int -> Ty) -> Int -> Ty -> Ty
mapTy onvar = go
  where
    go :: Int -> Ty -> Ty
    go _ TInt                 = TInt
    go c (TVar x)             = onvar x c
    go c (TRegFile qnts rfty) = TRegFile qnts $ rfty
                                    & rfRegTy %~ M.map (mapTy onvar (c + length qnts))
                                    & rfStackTy %~ (mapStackTy onvar (c + length qnts) <$>)
    go c (TExists ty)         = TExists (go (c + 1) ty)
    go c (TRecurs ty)         = TRecurs (go (c + 1) ty)
    go c (TRow row_ty)        = TRow $ mapRowTy onvar c row_ty
    go _ TNonsense            = TNonsense
    go c (TPtr st_ty)         = TPtr $ mapStackTy onvar c st_ty
    go _ (TAlias name)        = TAlias name

mapRowTy :: (Int -> Int -> Ty) -> Int -> RowTy -> RowTy
mapRowTy _ _ REmpty = REmpty
mapRowTy onvar c (RVar x) = case onvar x c of
        TRow row -> row
        TVar y   -> RVar y
        _        -> error "TRow or TVar required"
mapRowTy onvar c (RSeq ty row) = RSeq (mapTy onvar c ty) (mapRowTy onvar c row)

mapStackTy :: (Int -> Int -> Ty) -> Int -> StackTy -> StackTy
mapStackTy _ _ SNil = SNil
mapStackTy onvar c (SVar x) = case onvar x c of
        TPtr st_ty -> st_ty
        TVar y     -> SVar y
        _          -> error "TPtr or TVar required"
mapStackTy onvar c (SCons ty st) = SCons (mapTy onvar c ty) st
mapStackTy onvar c (SComp st1 st2) = SComp (mapStackTy onvar c st1) (mapStackTy onvar c st2)

shiftTy :: Int -> Ty -> Ty
shiftTy d = mapTy (\x c -> TVar (if x < c then x else x + d)) 0

substTy :: Ty -> Ty -> Ty
substTy s = mapTy (\x j -> if x == j then shiftTy j s else TVar x) 0

substTop :: Ty -> Ty -> Ty
substTop s t = shiftTy (-1) (substTy (shiftTy 1 s) t)
