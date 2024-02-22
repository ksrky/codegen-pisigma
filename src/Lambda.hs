{-# LANGUAGE TemplateHaskell #-}

module Lambda (
    Lit(..),
    Meta(..),
    Ty(..),
    TyF(..),
    Var,
    Exp(..),
    ExpF(..),
    Prog,
    Typeable(..),
    stripAnn) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.IORef
import GHC.Stack
import Id

newtype Lit = LInt Int
    deriving (Eq, Show)

newtype Meta = Meta (IORef (Maybe Ty))
    deriving (Eq)

instance Show Meta where
    show (Meta _) = "?m"

data Ty
    = TInt
    | TFun Ty Ty
    | TMeta Meta
    deriving (Eq, Show)

type Var = (Id, Ty)

data Exp
    = ELit Lit
    | EVar Var
    | EApp Exp Exp
    | ELam Var Exp
    | ELet Var Exp Exp
    | ELetrec [(Var, Exp)] Exp
    | EExpTy Exp Ty
    deriving (Eq, Show)

type Prog = Exp

makeBaseFunctor ''Ty
makeBaseFunctor ''Exp

class Typeable a where
    typeof :: HasCallStack => a -> Ty

instance Typeable Lit where
    typeof _ = TInt

instance Typeable Ty where
    typeof = id

instance Typeable Var where
    typeof = snd

instance Typeable Exp where
    typeof = cata $ \case
        ELitF l -> typeof l
        EVarF v -> typeof v
        EAppF t1 _ -> case t1 of
            TFun _ t12 -> t12
            _          -> error "impossible"
        ELamF (_, t1) t2 -> TFun t1 t2
        ELetF _ _ t -> t
        ELetrecF _ t -> t
        EExpTyF _ t -> t

stripAnn :: Exp -> Exp
stripAnn = cata $ \case
    EExpTyF e _ -> e
    e -> embed e
