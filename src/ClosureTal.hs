module ClosureTal where
{-{-# LANGUAGE TemplateHaskell #-}

module ClosureTal where

import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable
import Data.Map.Strict          qualified as M
import Id
import Prelude                  hiding (exp)
import Tal                      qualified as T

data Ctx = Ctx {
    _idReg      :: [(Id, T.Reg)],
    _idFuncName :: [(Id, T.Name)],
    _telescopes :: T.Telescopes,
    _regFileTy  :: T.RegFileTy
}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

closureTalTy :: C.Ty -> CtxM T.Ty
closureTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF i -> undefined
    C.TNameF x -> undefined
    C.TFunF ts1 _ -> T.TRegFile . T.mkRegFileTy <$> sequence ts1
    C.TExistsF _ t -> T.TExists <$> t
    C.TRecursF _ t -> T.TRecurs <$> t
    C.TRowF r -> undefined

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

lookupIdReg :: Id -> CtxM T.Reg
lookupIdReg x = do
    xrs <- view idReg
    case lookup x xrs of
        Just r  -> return r
        Nothing -> fail "unbound variable"

lookupIdFuncName :: Id -> CtxM T.Name
lookupIdFuncName f = do
    fns <- view idFuncName
    case lookup f fns of
        Just n  -> return n
        Nothing -> fail "unbound function"

-- Writer [T.Dec]
closureTalVal :: C.Val -> CtxM T.SmallVal
closureTalVal (C.VLit l)     = return $ T.VWord (closureTalLit l)
closureTalVal (C.VVar x)     = T.VReg <$> lookupIdReg (fst x)
closureTalVal (C.VFun _)     = fail ""
closureTalVal (C.VLabel _ _) = undefined
closureTalVal (C.VTuple {})  = undefined
closureTalVal (C.VPack t1 v t2) =
    T.VPack <$> closureTalTy t1 <*> closureTalVal v <*> closureTalTy t2
closureTalVal (C.VRoll v t) = T.VRoll <$> closureTalVal v <*> closureTalTy t
closureTalVal (C.VUnroll v) = T.VUnroll <$> closureTalVal v
closureTalVal (C.VAnnot v _) = closureTalVal v

data St = St {
    _nextReg :: T.Reg,
    _heaps   :: T.Heaps,
    _instrs  :: T.Instrs
}

makeLenses ''St

type StM = StateT St CtxM

freshReg :: StM T.Reg
freshReg = do
    r <- use nextReg
    nextReg .= T.Reg (T.unReg r + 1)
    return r

closureTalExp :: C.Exp -> StM ()
closureTalExp (C.ELet (C.BVal x v) e) = do
    r <- freshReg
    t <- lift $ closureTalTy (snd x)
    locally idReg ((fst x, r) :) $
        locally regFileTy (M.insert r t) $ closureTalExp e
    v' <- lift $ closureTalVal v
    instrs %= T.ISeq (T.IMove r v')
closureTalExp (C.ELet (C.BCall x v vs) e) | let arity = length vs = do
    r <- freshReg
    ty <- lift $ closureTalTy (snd x)
    locally idReg ((fst x, r) :) $
        locally regFileTy (M.insert r ty) $ closureTalExp e
    v' <- lift $ closureTalVal v
    vs' <- mapM (lift . closureTalVal) vs
    tmpRegs <- replicateM arity freshReg
    let argRegs = map T.Reg [1 .. arity]
        mv_instrs =
            zipWith T.IMove tmpRegs vs'
            ++ zipWith (\a t -> T.IMove a (T.VReg t)) argRegs tmpRegs
    instrs %= foldr ((.) . T.ISeq) (T.ISeq (T.ICall r v' argRegs)) mv_instrs
closureTalExp (C.ELet (C.BProj x v i) e) = do
    r <- freshReg
    t <- lift $ closureTalTy (snd x)
    locally idReg ((fst x, r) :) $
        locally regFileTy (M.insert r t) $ closureTalExp e
    v' <- lift $ closureTalVal v
    instrs %= T.ISeq (T.IMove r v') . T.ISeq (T.ILoad r r (i - 1))
closureTalExp (C.ELet (C.BUnpack _ x v) e) = do
    r <- freshReg
    t <- lift $ closureTalTy (snd x)
    locally idReg ((fst x, r) :) $ locally telescopes (0 :) $
        locally regFileTy (M.insert r t) $ closureTalExp e
    v' <- lift $ closureTalVal v
    instrs %= T.ISeq (T.IUnpack 0 r v') -- tmp: TyVar
closureTalExp (C.ECase v les) = do
    undefined
closureTalExp (C.EReturn v) = do
    v' <- lift $ closureTalVal v
    instrs .= T.ISeq (T.IMove T.retReg v') (T.IReturn v')
    undefined
closureTalExp (C.EAnnot e _) = closureTalExp e

closureTalDef :: C.Defn -> StM T.Heap
closureTalDef (C.Defn {C.args, C.body}) | let arity = length args = do
    let argRegs = map T.Reg [1 .. arity]
    rfty <- lift $ M.fromList . zip argRegs <$> mapM (closureTalTy . snd) args
    locally idReg (zip (map fst args) argRegs ++) $
        locally telescopes (const []) $
        locally regFileTy (const rfty) $ do
            closureTalExp body
            T.HCode rfty <$> use instrs

closureTalProgram :: C.Program -> IO T.Program
closureTalProgram (decs, defns, exp) = do
    undefined

-- for each heap
-- StateT (freeRegSet, Instrs)
-- ReaderT (RegFileTy, Telescopes, idReg)

-- ReaderT (idFuncName, idStruct, labelStruct)
-}