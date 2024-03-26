{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Anf (
    Lit(..),
    Ty(..),
    TyF(..),
    Var,
    ValF(..),
    Val(..),
    FunVal(..),
    Bind(..),
    RecBind(..),
    Exp(..),
    ExpF(..),
    Dec(..),
    Program,
    Env,
    lookupEnumEnv,
    lookupBindEnv,
    extendBindEnv,
    Typeable(..),
    bindVar
) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import PisigmaTal.Id

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Ty
    = TInt
    | TName Id
    | TFun [Ty] Ty
    | TTuple [Ty]
    deriving (Eq, Show)

type Var = (Id, Ty)

data Val
    = VLit Lit
    | VVar Var
    | VLabel Label Ty
    | VLam [Var] Exp
    | VTuple [Val]
    | VAnnot Val Ty
    deriving (Eq, Show)

data FunVal = LocalFun Val | ExternalFun Var
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BCall Var FunVal [Val]
    deriving (Eq, Show)

data RecBind = RecBind Var [Var] Exp
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ELetrec [RecBind] Exp
    | ECase Val [(Label, Exp)]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Dec
    = DEnum Id [Label]
    | DBind Id Ty
    deriving (Eq, Show)

type Program = ([Dec], Exp)

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
    typeof :: a -> Ty

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
        VLabelF _ t -> t
        VLamF xs e -> TFun (map typeof xs) (typeof e)
        VTupleF vs -> TTuple (map typeof vs)
        VAnnotF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ e ->  e
        ELetrecF _ e -> e
        ECaseF _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EReturnF v -> typeof v
        EAnnotF _ t -> t

bindVar :: Bind -> Var
bindVar (BVal x _)    = x
bindVar (BCall x _ _) = x
