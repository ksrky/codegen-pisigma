{-# LANGUAGE TemplateHaskell #-}

module Anf (
    Lit(..),
    Ty(..),
    TyF(..),
    Var,
    ValF(..),
    Val(..),
    Bind(..),
    Exp(..),
    ExpF(..),
    Dec(..),
    Prog,
    Env,
    lookupEnumEnv,
    lookupBindEnv,
    extendBindEnv,
    Typeable(..),
    getBindVar) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack
import Id

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Ty
    = TInt
    | TFun [Ty] Ty
    | TName Id
    deriving (Eq, Show)

type Var = (Id, Ty)

data Val
    = VLit Lit
    | VVar Var
    | VLab Label Ty
    | VLam [Var] Exp
    | VValTy Val Ty
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BCall Var Val [Val]
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ELetrec [Bind] Exp
    | ECase Val [(Label, Exp)]
    | ERet Val
    | EExpTy Exp Ty
    deriving (Eq, Show)

data Dec
    = DEnum Id [Label]
    | DBind Id Ty
    deriving (Eq, Show)

type Prog = Exp

makeBaseFunctor ''Ty
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

type Env = [Dec]

lookupEnumEnv :: MonadFail m => Id -> Env -> m [Label]
lookupEnumEnv x = \case
    DEnum y ls : _ | x == y -> return ls
    _ : env -> lookupEnumEnv x env
    [] -> fail "Enum not found"

lookupBindEnv :: MonadFail m => Id -> Env -> m Ty
lookupBindEnv x = \case
    DBind y t : _ | x == y -> return t
    _ : env -> lookupBindEnv x env
    [] -> fail "Func not found"

extendBindEnv :: Var -> Env -> Env
extendBindEnv (x, t) = (DBind x t:)

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
        VLabF _ t -> t
        VLamF xs e -> TFun (map typeof xs) (typeof e)
        VValTyF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ e ->  e
        ELetrecF _ e -> e
        ECaseF _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        ERetF v -> typeof v
        EExpTyF _ t -> t

getBindVar :: Bind -> Var
getBindVar (BVal x _)    = x
getBindVar (BCall x _ _) = x
