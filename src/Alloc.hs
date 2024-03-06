{-# LANGUAGE TemplateHaskell #-}

module Alloc (
    Name (..),
    Ty (..),
    TyF(..),
    RowTy(..),
    RowTyF(..),
    Const (..),
    Val (..),
    ValF (..),
    Bind (..),
    Exp (..),
    ExpF (..),
    Heap (..),
    Program,
    substTop,
    Typeable(..)
) where

import Control.Lens.At
import Control.Lens.Operators
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Idx

newtype Name = Name String
    deriving (Eq, Show)

type InitFlag = Bool

data Ty
    = TInt
    | TVar Int
    | TFun [Ty] Ty
    | TExists Ty
    | TRecurs Ty
    | TRow RowTy
    | TAlias Name (Maybe Ty)
    deriving (Eq, Show)

infixr 5 :>

data RowTy = REmpty | RVar Int | (Ty, InitFlag) :> RowTy
    deriving (Eq, Show)

type instance Index RowTy = Idx

type instance IxValue RowTy = (Ty, InitFlag)
instance Ixed RowTy where
    ix _ _ REmpty             = pure REmpty
    ix _ _ (RVar x)           = pure $ RVar x
    ix Idx1 f (ty :> row)     = f ty <&> (:> row)
    ix (IdxS k) f (ty :> row) = (ty :>) <$> ix k f row

data Const
    = CInt Int
    | CGlobal Name Ty
    deriving (Eq, Show)

data Val
    = VVar Int Ty
    | VConst Const
    | VPack Ty Val Ty
    | VRoll Val Ty
    | VUnroll Val
    | VAnnot Val Ty
    deriving (Eq, Show)

data Bind
    = BVal Ty Val
    | BCall Ty Val [Val]
    | BProj Ty Val Idx
    | BUnpack Ty Val
    | BMalloc Ty [Ty]
    | BUpdate Ty Val Idx Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [Exp]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Heap
    = HGlobal Ty Val
    | HCode [Ty] Ty Exp
    | HExtern Ty
    | HTypeAlias Ty
    deriving (Eq, Show)

type Program = ([(Name, Heap)], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

mapTy :: (Int -> Int -> Ty) -> Int -> Ty -> Ty
mapTy onvar = flip $ cata $ \case
    TIntF -> const TInt
    TVarF x -> onvar x
    TFunF arg_tys ret_ty -> \c -> TFun (map ($ c) arg_tys) (ret_ty c)
    TExistsF ty -> \c -> TExists (ty (c + 1))
    TRecursF ty -> \c -> TRecurs (ty (c + 1))
    TRowF row_ty -> \c -> TRow $ mapRowTy onvar c row_ty
    TAliasF name mb_ty -> \c -> TAlias name (mb_ty <*> pure (c + 1))

mapRowTy ::  (Int -> Int -> Ty) -> Int -> RowTy -> RowTy
mapRowTy onvar c = cata $ \case
    REmptyF -> REmpty
    RVarF x -> case onvar x c of TRow row -> row; _ -> error "TRow required"
    (ty, flag) :>$ row -> (mapTy onvar c ty, flag) :> row

shiftTy :: Int -> Ty -> Ty
shiftTy d = mapTy (\x c -> TVar (if x < c then x else x + d)) 0

substTy :: Ty -> Ty -> Ty
substTy s = mapTy (\x j -> if x == j then shiftTy j s else TVar x) 0

substTop :: Ty -> Ty -> Ty
substTop s t = shiftTy (-1) (substTy (shiftTy 1 s) t)

class Typeable a where
    typeof :: a -> Ty

instance Typeable Ty where
    typeof = id

instance Typeable Val where
    typeof = undefined

instance Typeable Exp where
    typeof = undefined
