{-# LANGUAGE TemplateHaskell #-}

module Alloc (
    Name (..),
    Ty (..),
    TyF(..),
    RowTy(..),
    RowTyF(..),
    Const (..),
    Val (..),
    ValF (..),
    Bind (..),
    Exp (..),
    ExpF (..),
    Heap (..),
    Program,
    shiftTy,
    substTop,
    Typeable(..)
) where

import Control.Lens.At
import Control.Lens.Operators
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Idx
import Prettyprinter            hiding (pretty)
import Prettyprinter.Prec

newtype Name = Name String
    deriving (Eq, Show)

type InitFlag = Bool

data Ty
    = TInt
    | TVar Int
    | TFun [Ty] Ty
    | TExists Ty
    | TRecurs Ty
    | TRow RowTy
    | TAlias Name (Maybe Ty)
    deriving (Eq, Show)

infixr 5 :>

data RowTy = REmpty | RVar Int | (Ty, InitFlag) :> RowTy
    deriving (Eq, Show)

type instance Index RowTy = Idx

type instance IxValue RowTy = (Ty, InitFlag)
instance Ixed RowTy where
    ix _ _ REmpty             = pure REmpty
    ix _ _ (RVar x)           = pure $ RVar x
    ix Idx1 f (ty :> row)     = f ty <&> (:> row)
    ix (IdxS k) f (ty :> row) = (ty :>) <$> ix k f row

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
    = BVal Ty Val
    | BCall Ty Val [Val]
    | BProj Ty Val Idx
    | BUnpack Ty Val
    | BMalloc Ty [Ty]
    | BUpdate Ty Val Idx Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [Exp]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Heap
    = HGlobal Ty Val
    | HCode [Ty] Ty Exp
    | HExtern Ty
    | HTypeAlias Ty
    deriving (Eq, Show)

type Program = ([(Name, Heap)], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

mapTy :: (Int -> Int -> Ty) -> Int -> Ty -> Ty
mapTy onvar = flip $ cata $ \case
    TIntF -> const TInt
    TVarF x -> onvar x
    TFunF arg_tys ret_ty -> \c -> TFun (map ($ c) arg_tys) (ret_ty c)
    TExistsF ty -> \c -> TExists (ty (c + 1))
    TRecursF ty -> \c -> TRecurs (ty (c + 1))
    TRowF row_ty -> \c -> TRow $ mapRowTy onvar c row_ty
    TAliasF name mb_ty -> \c -> TAlias name (mb_ty <*> pure (c + 1))

mapRowTy ::  (Int -> Int -> Ty) -> Int -> RowTy -> RowTy
mapRowTy onvar c = cata $ \case
    REmptyF -> REmpty
    RVarF x -> case onvar x c of
        TRow row -> row
        TVar y   -> RVar y
        _        -> error "TRow or TVar required"
    (ty, flag) :>$ row -> (mapTy onvar c ty, flag) :> row

shiftTy :: Int -> Ty -> Ty
shiftTy d = mapTy (\x c -> TVar (if x < c then x else x + d)) 0

substTy :: Ty -> Ty -> Ty
substTy s = mapTy (\x j -> if x == j then shiftTy j s else TVar x) 0

substTop :: Ty -> Ty -> Ty
substTop s t = shiftTy (-1) (substTy (shiftTy 1 s) t)

class Typeable a where
    typeof :: a -> Ty

instance Typeable Ty where
    typeof = id

instance Typeable Val where
    typeof = undefined

instance Typeable Exp where
    typeof = undefined

instance PrettyPrec Name where
    pretty (Name s) = pretty s

instance PrettyPrec Const where
    pretty (CInt i)      = pretty i
    pretty (CGlobal f _) = pretty f

instance PrettyPrec Ty where
    prettyPrec _ TInt = "Int"
    prettyPrec _ (TVar i) = "`" <> pretty i
    prettyPrec p (TFun ts t) = parPrec p 2 $
        parens (hsep $ punctuate "," $ map (prettyPrec 1) ts) <+> "->" <+> prettyPrec 2 t
    prettyPrec p (TExists t) = parPrec p 0 $ "∃_" <> dot <+> pretty t
    prettyPrec p (TRecurs t) = parPrec p 0 $ "μ_" <> dot <+> pretty t
    prettyPrec _ (TRow r) = braces $ pretty r
    prettyPrec _ (TAlias name _) = pretty name

instance PrettyPrec RowTy where
    pretty REmpty               = "ε"
    pretty (RVar i)             = "`" <> pretty i
    pretty ((ty, True) :> row)  = pretty ty <> "¹" <> "," <+> pretty row
    pretty ((ty, False) :> row) = pretty ty <> "⁰" <> "," <+> pretty row

instance PrettyPrec Val where
    prettyPrec _ (VVar i _)    = "`" <> pretty i
    prettyPrec _ (VConst c)    = pretty c
    prettyPrec p (VPack t1 v t2) = parPrec p 0 $ hang 2 $
        hsep ["pack", brackets (pretty t1 <> "," <+> pretty v) <> softline <> "as", prettyPrec 2 t2]
    prettyPrec p (VRoll v t)   = parPrec p 0 $ hang 2 $ hsep ["roll", prettyMax v <> softline <> "as", prettyPrec 2 t]
    prettyPrec p (VUnroll v)   = parPrec p 0 $ "unroll" <+> prettyPrec 1 v
    prettyPrec _ (VAnnot v t)  = parens $ hang 2 $ sep [pretty v, ":" <+> pretty t]

instance PrettyPrec Bind where
    pretty (BVal _ v) = "_ =" <+> pretty v
    pretty (BCall _ f vs) = "_ =" <+> pretty f <+> hsep (map pretty vs)
    pretty (BProj _ v i) = "_ =" <+> pretty v <> "." <> pretty i
    pretty (BUnpack t v) = "[_, _ :" <+> pretty t <> "] = unpack" <+> pretty v
    pretty (BMalloc _ ts) = "_ = malloc" <+> brackets (hsep (punctuate ", " (map pretty ts)))
    pretty (BUpdate _ v1 i v2) = "_ =" <+> pretty v1 <> brackets (pretty i) <+> "<-" <+> pretty v2

instance PrettyPrec Exp where
    pretty (ELet b e)   = vsep [hang 2 ("let" <+> pretty b) <+> "in", pretty e]
    pretty (ECase v es) = vsep [ "case" <+> pretty v <+> "of"
                               , "  " <> align (vsep (map (\ei -> hang 2 $ sep ["_ ->", pretty ei]) es))]
    pretty (EReturn v)  = "ret" <+> prettyMax v
    pretty (EAnnot e t) = parens $ hang 2 $ sep [pretty e, ":" <+> pretty t]

instance PrettyPrec (Name, Heap) where
    pretty (name, HGlobal t v) = pretty name <+> ":" <+> pretty t <+> "=" <+> pretty v
    pretty (name, HCode ts1 t2 e) = pretty name <+> "="
        <+> parens (hsep $ punctuate ", " $ map (\t -> "_ :" <+> pretty t) ts1)
        <+> ":" <+> pretty t2 <+> "=" <+> pretty e
    pretty (name, HExtern t) = "extern" <+> pretty name <+> "=" <+> pretty t
    pretty (name, HTypeAlias t) = "type" <+> pretty name <+> "=" <+> pretty t

instance PrettyPrec Program where
    pretty (hs, e) = vsep (map pretty hs) <> line <> pretty e
