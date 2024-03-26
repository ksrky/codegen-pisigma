module Tal.Prettyprint (PprTal(..)) where

import Data.Map.Strict qualified as M
import Prettyprinter
import Tal.Syntax

class PprTal a where
    pprtal :: a -> Doc ann

instance PprTal Reg where
    pprtal (GeneralReg i) = "r" <> pretty i
    pprtal (SpecialReg s) = pretty s

instance PprTal Name where
    pprtal (Name s u) = pretty s <> "_" <> pretty u

instance PprTal TyVar where
    pprtal = pretty

instance PprTal Ty where
    pprtal TInt            = "int"
    pprtal (TVar i)        = "#" <> pretty i
    pprtal (TRegFile rfty) = pprtal rfty
    pprtal (TExists ty)    = "∃." <+> pprtal ty
    pprtal (TRecurs ty)    = "μ." <+> pprtal ty
    pprtal (TRow rty)      = pprtal rty
    pprtal TNonsense       = "ns"
    pprtal (TPtr sty)      = "ptr" <+> pprtal sty
    pprtal (TAlias name)   = pprtal name

instance PprTal RowTy where
    pprtal REmpty                = "ε"
    pprtal (RVar i)              = "#" <> pretty i
    pprtal (RSeq (ty, flag) rty) =
        parens (pprtal ty) <> pretty (fromEnum flag) <+> ", " <+> pprtal rty

instance PprTal StackTy where
    pprtal SNil           = "nil"
    pprtal (SVar i)       = "#" <> pretty i
    pprtal (SCons ty sty) = pprtal ty <+> "::" <+> pprtal sty

instance PprTal RegFileTy where
    pprtal rfty = encloseSep "{" "}" ", " $
        map (\(reg, ty) -> pprtal reg <> ":" <+> pprtal ty) (M.toList rfty)

instance PprTal (Val a) where
    pprtal (VReg r)   = pprtal r
    pprtal (VWord w)  = pprtal w
    pprtal (VLabel l) = pprtal l
    pprtal (VInt i)   = pretty i
    pprtal (VJunk ty) = "?" <> pprtal ty
    pprtal (VPack ty v ty') =
        "pack" <+> brackets (pprtal ty <> ", " <+> pprtal v) <+> "as" <+> pprtal ty'
    pprtal (VRoll v ty) = "roll" <+> parens (pprtal v) <+> "as" <+> pprtal ty
    pprtal (VUnroll v)  = "unroll" <+> parens (pprtal v)
    pprtal VNonsense     = "nonsense"
    pprtal (VPtr i)      = "ptr" <> parens (pretty i)

instance PprTal (Name, Heap) where
    pprtal (name, HGlobal word) = "global" <+> pprtal name <+> "=" <+> pprtal word
    pprtal (name, HCode tvs rfty instrs) = pprtal name <+> "=" <+> "code" <>
        encloseSep "[" "]" ", " (map pprtal tvs) <> pprtal rfty <> "." <> line <> pprtal instrs
    pprtal (name, HStruct ws) = "struct" <+> pprtal name <+> "=" <+>
        encloseSep "{" "}" ", " (map pprtal ws)

instance PprTal Aop where
    pprtal Add = "add"
    pprtal Sub = "sub"
    pprtal Mul = "mul"

instance PprTal Bop where
    pprtal Bz  = "bz"
    pprtal Bnz = "bnz"
    pprtal Bgt = "bgt"
    pprtal Blt = "blt"

instance PprTal Instr where
    pprtal (IAop aop r1 r2 v) = pprtal aop <+> pprtal r1 <> "," <+> pprtal r2 <> "," <+> pprtal v
    pprtal (IBop bop r v) = pprtal bop <+> pprtal r <> "," <+> pprtal v
    pprtal (ICall v) = "call" <+> pprtal v
    pprtal (ILoad r1 r2 i) = "ld" <+> pprtal r1 <> "," <+> pprtal r2 <> parens (pretty i)
    pprtal (IMalloc r tys) = "malloc" <+> pprtal r <+> encloseSep "[" "]" "," (map pprtal tys)
    pprtal (IMove r v) = "mv" <+> pprtal r <> "," <+> pprtal v
    pprtal (IStore r1 i r2) = "st" <+> pprtal r1 <> parens (pretty i) <> "," <+> pprtal r2
    pprtal (IUnpack r v) = "unpack" <+> pprtal r <> "," <+> pprtal v
    pprtal (ISalloc n) = "salloc" <+> pretty n
    pprtal (ISfree n) = "sfree" <+> pretty n
    pprtal (ISload r1 r2 i) = "sld" <+> pprtal r1 <> "," <+> pprtal r2 <> parens (pretty i)
    pprtal (ISstore r1 i r2) = "sst" <+> pprtal r1 <> parens (pretty i) <> "," <+> pprtal r2

instance PprTal Instrs where
    pprtal (ISeq instr instrs) = vsep [pprtal instr, pprtal instrs]
    pprtal (IJump v)           = "jmp" <+> pprtal v
    pprtal (IHalt ty)          = "halt" <+> brackets (pprtal ty)

instance PprTal Program where
    pprtal (heaps, instrs) = vsep (map pprtal (M.toList heaps)) <> line <> pprtal instrs
