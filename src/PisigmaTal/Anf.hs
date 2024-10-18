{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Anf (
    Lit(..),
    Ty(..),
    TyF(..),
    Var,
    ValF(..),
    Val(..),
    Op(..),
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
    StripAnnot(..),
    bindVar
) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import PisigmaTal.Id
import PisigmaTal.Primitive

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
    | VLabel EnumId Label
    | VLam [Var] Exp
    | VTuple [Val]
    | VAnnot Val Ty
    deriving (Eq, Show)

data Op
    = KnownOp Id Ty
    | PrimOp PrimOp Ty
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BApp Var Val [Val]
    | BFullApp Var Op [Val]
    deriving (Eq, Show)

data RecBind = RecBind Var [Var] Exp
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ELetrec [RecBind] Exp
    | ECase EnumId Val [(Label, Exp)]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

type EnumId = Id

data Dec
    = DEnum EnumId [Label]
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
        VLabelF c _ -> TName c
        VLamF xs e -> TFun (map typeof xs) (typeof e)
        VTupleF vs -> TTuple (map typeof vs)
        VAnnotF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ e ->  e
        ELetrecF _ e -> e
        ECaseF _ _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EReturnF v -> typeof v
        EAnnotF _ t -> t

class StripAnnot a where
    stripAnnotTop :: a -> a

instance StripAnnot Val where
    stripAnnotTop (VAnnot v _) = stripAnnotTop v
    stripAnnotTop v            = v

bindVar :: Bind -> Var
bindVar (BVal x _)       = x
bindVar (BApp x _ _)     = x
bindVar (BFullApp x _ _) = x
