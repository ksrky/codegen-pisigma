module Asm where

import Data.Word

newtype Name = Name String
    deriving (Eq, Show)

data Type
    = TArray  Type Word32
    | TFunc   Type [Type]
    | TInt    Word32
    | TPtr    Type
    | TStruct [Type]
    | TVoid
    deriving (Eq, Show)

data Const
    = GlobalRef Type Name
    | Integer Word32 Integer
    | Array Type [Const]
    | Struct (Maybe Name) [Const]
    deriving (Eq, Show)

data Val
    = Reg Type Name
    | Const Const
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
    | EGep Type Val [Val]
    | EMalloc Type
    | EStore Val Val
    deriving (Eq, Show)

data Stm
    = SSeq (Named Exp) Stm
    | SBranch Name
    | SCondBranch Val Name Name
    | SJump Name
    | SReturn Val
    | SSwitch Val Name [(Const, Name)]
    deriving (Eq, Show)

data Named a = Name := a | Do a
    deriving (Eq, Show)

newtype BasicBlock = BasicBlock Stm
    deriving (Eq, Show)

data Module = Module
    { moduleName     :: String
    , moduleFileName :: String
    } deriving (Eq, Show)
