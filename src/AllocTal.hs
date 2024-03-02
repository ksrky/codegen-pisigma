{-# LANGUAGE TemplateHaskell #-}

module AllocTal where

import Alloc                    qualified as A
import Tal                      qualified as T

import Control.Lens.Combinators
import Control.Monad.Reader
import Data.Functor.Foldable

data Ctx = Ctx {
    _idReg      :: [T.Reg],
    _idFuncName :: [(A.Name, T.Name)],
    _telescopes :: T.Telescopes,
    _regFileTy  :: T.RegFileTy
}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

allocTalTy :: A.Ty -> CtxM T.Ty
allocTalTy = cata $ \case
    A.TIntF -> return T.TInt
    A.TVarF i -> return $ T.TVar i
    A.TFunF ts1 _ -> T.TRegFile . T.mkRegFileTy <$> sequence ts1
    A.TExistsF t -> T.TExists <$> t
    A.TRecursF t -> T.TRecurs <$> t
    A.TStructF r -> undefined
    A.TAliasF n  -> undefined

allocConst :: A.Const -> T.WordVal
allocConst (A.CInt i)               = T.VInt i
allocConst (A.CGlobal (A.Name s) _) = T.VLabel (T.Name s)

allocTalVal :: A.Val -> CtxM T.SmallVal
allocTalVal (A.VVar x t) = undefined
allocTalVal (A.VConst c) = return $ T.VWord (allocConst c)
allocTalVal (A.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (A.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (A.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (A.VAnnot v _) = allocTalVal v

