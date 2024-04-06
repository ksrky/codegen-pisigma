{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Closure (
    Lit(..),
    TyVar,
    Ty(..),
    TyF(..),
    RowTy(..),
    RowTyF(..),
    Var,
    Label,
    Val(..),
    ValF(..),
    Bind(..),
    Exp(..),
    ExpF(..),
    Code(..),
    Defn,
    TopExp,
    EnumId,
    Dec(..),
    Program,
    Env,
    lookupEnumEnv,
    lookupBindEnv,
    extendBindEnv,
    pattern ClosTy,
    pattern UClosTy,
    unrollUClosTy,
    unpackClosTy,
    bindVar,
    Typeable(..),
    StripAnnot(..),
    mkTTuple
) where

import Control.Lens.At
import Control.Lens.Cons
import Control.Lens.Operators
import Control.Lens.Prism
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack
import PisigmaTal.Id
import PisigmaTal.Idx
import PisigmaTal.Primitive
import Prettyprinter            hiding (Pretty (..))
import Prettyprinter.Prec

type TyVar = Id

data Ty
    = TInt
    | TVar TyVar
    | TName Id
    | TFun [Ty] Ty
    | TExists TyVar Ty
    | TRecurs TyVar Ty
    | TRow RowTy
    deriving (Eq, Show)

data RowTy = REmpty | RVar TyVar | RSeq Ty RowTy
    deriving (Eq, Show)

type instance Index RowTy = Idx

type instance IxValue RowTy = Ty
instance Ixed RowTy where
    ix _ _ REmpty               = pure REmpty
    ix _ _ (RVar x)             = pure $ RVar x
    ix Idx1 f (RSeq ty row)     = f ty <&> (`RSeq` row)
    ix (IdxS k) f (RSeq ty row) = RSeq ty <$> ix k f row

instance Cons RowTy RowTy Ty Ty where
    _Cons = prism (uncurry RSeq) $ \case
        RSeq ty row -> Right (ty, row)
        row         -> Left row

newtype Lit = LInt Int
    deriving (Eq, Show)

type Var = (Id, Ty)

type Label = String

data Val
    = VLit Lit
    | VVar Var
    | VFun Var
    | VLabel EnumId Label
    | VPack Ty Val Ty
    | VRoll Val Ty
    | VUnroll Val
    | VAnnot Val Ty
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BCall Var Var [Val]
    | BOpCall Var PrimOp Ty [Val]
    | BProj Var Val Idx
    | BUnpack TyVar Var Val
    | BMalloc Var [Ty]
    | BUpdate Var Var Idx Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase EnumId Val [(Label, Exp)]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Code = Code {args :: [Var], body :: Exp}
    deriving (Eq, Show)

type Defn = (Var, Code)

type EnumId = Id

data Dec
    = DEnum EnumId [Label]
    | DBind Id Ty
    deriving (Eq, Show)

type TopExp = ([Defn], Exp)

type Program = ([Dec], TopExp)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy
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

mkTTuple :: [Ty] -> Ty
mkTTuple ts = TRow $ foldr (:<) REmpty ts

pattern ClosTy :: [Ty] -> Ty -> Ty
pattern ClosTy ts1 t2 <- TExists _ (TRecurs _ (TRow (TFun (_ : ts1) t2 :< RVar _))) where
    ClosTy ts1 t2 =
        let t_env = newIdUnsafe "t_env"
            t_cl  = newIdUnsafe "t_cl" in
        TExists t_env $ TRecurs t_cl $ TRow $ TFun (TVar t_cl : ts1) t2 :< RVar t_env

pattern UClosTy :: [Ty] -> Ty -> RowTy -> Ty
pattern UClosTy ts1 t2 r <- TRecurs _ (TRow (TFun (_ : ts1) t2 :< r)) where
    UClosTy ts1 t2 r =
        let t_cl = newIdUnsafe "t_cl" in
        TRecurs t_cl $ TRow (TFun (TVar t_cl : ts1) t2 :< r)

unpackClosTy :: Ty -> Ty -> Ty
unpackClosTy (TRow r) (ClosTy ts1 t2) = UClosTy ts1 t2 r
unpackClosTy  _ _                     = error "existential type required"

unrollUClosTy :: Ty -> Ty
unrollUClosTy s@(UClosTy ts1 t2 r) = TRow $ TFun (s : ts1) t2 :< r
unrollUClosTy _                    = error "recursive type required"

bindVar :: Bind -> Var
bindVar (BVal x _)        = x
bindVar (BCall x _ _)     = x
bindVar (BOpCall x _ _ _) = x
bindVar (BProj x _ _)     = x
bindVar (BUnpack _ x _)   = x
bindVar (BMalloc x _)     = x
bindVar (BUpdate x _ _ _) = x

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
        VFunF f -> typeof f
        VLabelF c _ -> TName c
        VPackF _ _ t -> t
        VRollF _ t -> t
        VUnrollF v ->
            case typeof v of
                TRecurs tv t -> unrollUClosTy (TRecurs tv t)
                _            -> error "required recursive type"
        VAnnotF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ t -> t
        ECaseF _ _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EReturnF v -> typeof v
        EAnnotF _ t -> t

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Ty where
    prettyPrec _ TInt = "Int"
    prettyPrec _ (TVar tv) = pretty tv
    prettyPrec _ (TName l) = pretty l
    prettyPrec p (TFun ts t) = parPrec p 2 $
        parens (hsep $ punctuate "," $ map (prettyPrec 1) ts) <+> "->" <+> prettyPrec 2 t
    prettyPrec p (TExists tv t) = parPrec p 0 $ "∃" <> pretty tv <> dot <+> pretty t
    prettyPrec p (TRecurs tv t) = parPrec p 0 $ "μ" <> pretty tv <> dot <+> pretty t
    prettyPrec _ (TRow r) = braces $ pretty r

instance PrettyPrec RowTy where
    pretty REmpty          = "ε"
    pretty (RVar tv)       = pretty tv
    pretty (RSeq t REmpty) = pretty t
    pretty (RSeq t r)      = pretty t <> "," <+> pretty r

instance PrettyPrec Var where
    pretty (x, t) = pretty x <+> ":" <+> pretty t

instance PrettyPrec Val where
    prettyPrec _ (VLit l) = pretty l
    prettyPrec _ (VVar (x, _)) = pretty x
    prettyPrec _ (VFun (f, _)) = pretty f
    prettyPrec _ (VLabel l _) = "`" <> pretty l
    prettyPrec p (VPack t1 v t2) =
        parPrec p 0 $ hang 2 $ hsep ["pack", brackets (pretty t1 <> "," <+> pretty v) <> softline <> "as", prettyPrec 2 t2]
    prettyPrec p (VRoll v t) = parPrec p 0 $ hang 2 $ hsep ["roll", prettyMax v <> softline <> "as", prettyPrec 2 t]
    prettyPrec p (VUnroll v) = parPrec p 0 $ "unroll" <+> prettyPrec 1 v
    prettyPrec _ (VAnnot v t) = parens $ hang 2 $ sep [pretty v, ":" <+> pretty t]

instance PrettyPrec Bind where
    pretty (BVal x v) = hang 2 $ pretty x <+> "=" <> softline <> pretty v
    pretty (BCall x (f, _) vs2) = hang 2 $ pretty x <+> "=" <> softline <> pretty f
        <> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (BOpCall x op _ vs) = hang 2 $ pretty x <+> "=" <> softline <> pretty op
        <> parens (hsep (punctuate "," (map prettyMax vs)))
    pretty (BProj x v idx) = hang 2 $ pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty idx
    pretty (BUnpack tv x v) = hang 2 $ brackets (pretty tv <> "," <+> pretty x) <+> "=" <> softline <> "unpack" <+> prettyMax v
    pretty (BMalloc x ts) = hang 2 $ pretty x <+> "=" <> softline <> "malloc" <+> brackets (hsep $ punctuate "," $ map pretty ts)
    pretty (BUpdate x v idx v') = hang 2 $ pretty x <+> "=" <> softline <> pretty v <+> "[" <> pretty idx <> "]" <+> "<-" <> softline <> pretty v'

instance PrettyPrec Exp where
    pretty (ELet b e)    = vsep [hang 2 ("let" <+> pretty b) <+> "in", pretty e]
    pretty (ECase _ v les) = vsep [ "case" <+> pretty v <+> "of"
                                , "  " <> align (vsep (map (\(li, ei) -> hang 2 $ sep [pretty li <+> "->", pretty ei]) les))]
    pretty (EReturn v)   = "ret" <+> prettyMax v
    pretty (EAnnot e t)  = parens $ hang 2 $ sep [pretty e, ":" <+> pretty t]

instance PrettyPrec Code where
    pretty (Code{args, body}) = "code" <> parens (hsep (punctuate "," (map prettyMax args)))
        <> "." <> line <> indent 2 (pretty body)

instance PrettyPrec Dec where
    pretty (DEnum x ls) = "enum" <+> pretty x <+> "=" <+> brackets (hsep $ punctuate "," $ map pretty ls)
    pretty (DBind x t)  = pretty x <+> ":" <+> pretty t

instance PrettyPrec TopExp where
    pretty (defns, e) = vsep (map (\((f, _), c) -> pretty f <+> "=" <+> pretty c) defns) <> line
        <> "main" <+> "=" <> line <> indent 2 (pretty e)

instance PrettyPrec Program where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e

class StripAnnot a where
    stripAnnot :: a -> a

instance StripAnnot Val where
    stripAnnot = cata $ \case
        VAnnotF v _ -> v
        v           -> embed v

instance StripAnnot Bind where
    stripAnnot (BVal x v)           = BVal x (stripAnnot v)
    stripAnnot (BCall x f vs)       = BCall x f (map stripAnnot vs)
    stripAnnot (BOpCall x op ty vs) = BOpCall x op ty (map stripAnnot vs)
    stripAnnot (BProj x v i)        = BProj x (stripAnnot v) i
    stripAnnot (BUnpack tv x v)     = BUnpack tv x (stripAnnot v)
    stripAnnot (BMalloc x ts)       = BMalloc x ts
    stripAnnot (BUpdate x y i v)    = BUpdate x y i (stripAnnot v)

instance StripAnnot Exp where
    stripAnnot = cata $ \case
        ELetF b e -> ELet (stripAnnot b) e
        ECaseF c v lts -> ECase c (stripAnnot v) lts
        EReturnF v -> EReturn (stripAnnot v)
        EAnnotF e _ -> e

instance StripAnnot Code where
    stripAnnot (Code xs e) = Code xs (stripAnnot e)

-- DDefns defns -> DDefns $ map (\(f, c) -> (f, stripAnnot c)) defns

instance StripAnnot TopExp where
    stripAnnot (defns, e) = (map (\(f, c) -> (f, stripAnnot c)) defns, stripAnnot e)

instance StripAnnot Program where
    stripAnnot (decs, e) = (decs, stripAnnot e)
