{-# LANGUAGE TemplateHaskell #-}

module Anf (
    Lit(..),
    Ty(..),
    TyF(..),
    Var,
    ValF(..),
    Val(..),
    Dec(..),
    Exp(..),
    ExpF(..),
    Prog,
    Typeable(..)) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack
import Id

newtype Lit = LInt Int
    deriving (Eq, Show)

data Ty
    = TInt
    | TFun [Ty] Ty
    deriving (Eq, Show)

type Var = (Id, Ty)

data Val
    = VLit Lit
    | VVar Var
    | VLam [Var] Exp
    | VValTy Val Ty
    deriving (Eq, Show)

data Dec
    = DVal Var Val
    | DCall Var Val [Val]
    deriving (Eq, Show)

data Exp
    = ELet Dec Exp
    | ELetrec [Dec] Exp
    | ERet Val
    | EExpTy Exp Ty
    deriving (Eq, Show)

type Prog = Exp

makeBaseFunctor ''Ty
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

class Typeable a where
    typeof :: HasCallStack => a -> Ty

instance Typeable Lit where
    typeof _ = TInt

instance Typeable Ty where
    typeof = id

instance Typeable Var where
    typeof = snd

instance Typeable Val where
    typeof = cata $ \case
        VLitF l -> typeof l
        VVarF x -> typeof x
        VLamF xs e -> TFun (map typeof xs) (typeof e)
        VValTyF _ t -> t

instance Typeable Dec where
    typeof (DVal _ v) = typeof v
    typeof (DCall _ e1 _) = case typeof e1 of
            TFun _ t12 -> t12
            _          -> error "impossible"

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ e ->  e
        ELetrecF _ e -> e
        ERetF v -> typeof v
        EExpTyF _ t -> t
