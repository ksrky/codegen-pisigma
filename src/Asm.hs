module Asm where

import Data.Word

newtype Name = Name String
    deriving (Eq, Show)

data Ty
    = TArray  Ty Word32
    | TFun   Ty [Ty]
    | TInt    Word32
    | TPtr    (Maybe Ty)
    | TStruct (Maybe Name) [Ty]
    | TVoid
    deriving (Eq, Show)

data Const
    = GlobalRef Ty Name
    | Integer Word32 Integer
    | Array Ty [Const]
    | Struct (Maybe Name) [Const]
    deriving (Eq, Show)

data Lit
    = LInt Word32 Integer
    deriving (Eq, Show)

data Val
    = VLit Lit
    | VReg Ty Int
    | VGlb Ty Name
    deriving (Eq, Show)

data ArithOp = ADD | SUB | MUL | DIV
    deriving (Eq, Show)

data CmpOp
    = EQ | NE | UGT | UGE | ULT
    | ULE | SGT | SGE | SLT | SLE
    deriving (Eq, Show)

data Exp
    = EArith ArithOp Val Val
    | ECall Val [Val]
    | ECmp CmpOp Val Val
    | EProj Ty Val Int
    | EMalloc Ty
    | EStore Val Val
    deriving (Eq, Show)

data Stm
    = SSeq (Named Exp) Stm
    | SBranch Name
    | SCondBranch Val Name Name
    | SJump Name
    | SReturn Val
    | SSwitch Val Name [(Int, Name)]
    deriving (Eq, Show)

data Named a = Name := a | Do a
    deriving (Eq, Show)

newtype BasicBlock = BasicBlock Stm
    deriving (Eq, Show)

data Module = Module
    { moduleName     :: String
    , moduleFileName :: String
    } deriving (Eq, Show)
