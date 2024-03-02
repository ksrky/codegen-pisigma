{-# LANGUAGE TemplateHaskell #-}

module Alloc (
    Name (..),
    Ty (..),
    TyF(..),
    Const (..),
    Val (..),
    ValF (..),
    Bind (..),
    Exp (..),
    ExpF (..),
    Heap (..),
    Program
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
    | TStruct [(Ty, InitFlag)]
    | TAlias Name
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
    = BVal Val
    | BCall Val [Val]
    | BProj Val Int
    | BUnpack Val
    | BMalloc [Ty]
    | BUpdate Val Int Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [Exp]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Heap
    = HVal Ty Val
    | HCode [Ty] Ty Exp
    | HExtern Ty
    | HTypeAlias Ty
    deriving (Eq, Show)

type Program = ([(Name, Heap)], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Val
makeBaseFunctor ''Exp
