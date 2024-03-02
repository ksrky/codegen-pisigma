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
    Typeable(..)
) where

import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))

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
    | TAlias Name
    deriving (Eq, Show)

infixr 5 :>

data RowTy = REmpty | RVar Int | (Ty, InitFlag) :> RowTy
    deriving (Eq, Show)

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
    | BProj Ty Val Int
    | BUnpack Ty Val
    | BMalloc Ty [Ty]
    | BUpdate Ty Val Int Val
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

class Typeable a where
    typeof :: a -> Ty

instance Typeable Ty where
    typeof = id

instance Typeable Val where
    typeof = undefined

instance Typeable Exp where
    typeof = undefined
