module Nameless (
    Global (..),
    Ty (..),
    Const (..),
    Val (..),
    Bind (..),
    Exp (..),
    Heap (..),
    Program
) where

newtype Global = Global String
    deriving (Eq, Show)

type InitFlag = Bool

data Ty
    = TInt
    | TVar Int
    | TFun [Ty] Ty
    | TExists Ty
    | TRecurs Ty
    | TStruct [(Ty, InitFlag)]
    | TAlias Global
    deriving (Eq, Show)

data Const
    = CInt Int
    | CGlobal Global
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
    | BUnpack Ty Val
    | BMalloc [Ty]
    | BUpdate Val Int Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [(Int, Exp)]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Heap
    = HVal Ty Val
    | HCode [Ty] Ty Exp
    | HStruct [Val]
    | HExtern Ty
    | HTypeAlias Ty
    deriving (Eq, Show)

type Program = ([(Global, Heap)], Exp)
