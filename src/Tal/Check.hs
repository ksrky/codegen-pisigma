{-# LANGUAGE TemplateHaskell #-}

module Tal.Check (checkProgram) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Map.Strict          qualified as M
import Tal.Constant
import Tal.Constructors
import Tal.Syntax

data TalState = TalState
    { _talHeaps   :: HeapsTy
    , _talRegFile :: RegFileTy
    , _talQuants  :: Quants
    }

makeLenses ''TalState

refresh :: StateT TalState IO ()
refresh = do
    talRegFile .= emptyRegFileTy
    talQuants .= []

lookupHeap :: Name -> StateT TalState IO Ty
lookupHeap name = do
    heaps <- use talHeaps
    case M.lookup name heaps of
        Just ty -> return ty
        Nothing -> fail $ "heap not found: " ++ show name

class TyEquiv a where
    infixl 2 =?, >>=?
    (=?) :: MonadFail m => a -> a -> m ()
    (>>=?) :: MonadFail m => m a -> a -> m ()
    ma >>=? a = ma >>= \a' -> a' =? a

class TySubrel a where
    infixl 2 <=?
    (<=?) :: MonadFail m => a -> a -> m ()

instance TyEquiv Ty where
    TInt =? TInt = return ()
    TVar x =? TVar y | x == y = return ()
    TRegFile qs1 rf1 =? TRegFile qs2 rf2 | length qs1 == length qs2 = rf1 =? rf2
    TExists t1 =? TExists t2 = t1 =? t2
    TRecurs t1 =? TRecurs t2 = t1 =? t2
    TRow r1 =? TRow r2 = r1 =? r2
    TNonsense =? TNonsense = return ()
    TPtr sty1 =? TPtr sty2 = sty1 =? sty2
    TAlias x =? TAlias y | x == y = return () -- tmp
    t1 =? t2 = fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

instance TySubrel Ty where
    TRegFile _ rf1 <=? TRegFile _ rf2 = rf1 <=? rf2 -- TODO: is it OK to discard quantifiers?
    ty1 <=? ty2                       = ty1 =? ty2

instance TyEquiv RowTy where
    REmpty =? REmpty = return ()
    RVar x =? RVar y | x == y = return ()
    RSeq t1 r1 =? RSeq t2 r2 = t1 =? t2 >> r1 =? r2
    r1 =? r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

instance TyEquiv RegFileTy where
    rf1 =? rf2 = do
        sequence_ [ ty1 =? ty2
            | (r1, ty1) <- M.toList (rf1 ^. rfRegTy)
            , (r2, ty2) <- M.toList (rf2 ^. rfRegTy)
            , r1 == r2 ]
        rf1 ^. rfStackTy =? rf2 ^. rfStackTy

instance TySubrel RegFileTy where
    rf1 <=? rf2 = do
        sequence_ [ ty1 =? ty2
            | (r2, ty2) <- M.toList (rf2 ^. rfRegTy)
            , ty1 <- toList $ M.lookup r2 (rf1 ^. rfRegTy) ]
        rf1 ^. rfStackTy =? rf2 ^. rfStackTy

instance TyEquiv StackTy where
    SNil =? SNil = return ()
    SVar x =? SVar y | x == y = return ()
    SCons t1 s1 =? SCons t2 s2 = t1 =? t2 >> s1 =? s2
    s1 =? s2 = fail $ "stack type mismatch. expected: " ++ show s1 ++ ", got: " ++ show s2

instance TyEquiv (Maybe StackTy) where
    Just s1 =? Just s2 = s1 =? s2
    _ =? _             = return ()

checkReg :: Reg -> StateT TalState IO Ty
checkReg reg =  do
    rfty <- use talRegFile
    case M.lookup reg (rfty ^. rfRegTy) of
        Just ty -> return ty
        Nothing -> fail $ "register not found: " ++ show reg

checkVal :: Val a -> StateT TalState IO Ty
checkVal (VReg reg) = checkReg reg
checkVal (VWord wval) = checkVal wval
checkVal (VLabel l) = lookupHeap l
checkVal (VInt _) = return TInt
checkVal (VJunk ty) = return ty
checkVal (VPack ty1 wval ann_ty)
    | TExists ty2 <- ann_ty = do
        ty <- checkVal wval
        substTop ty1 ty2 =? ty
        return ann_ty
    | otherwise = fail $ "expected existential type, but got " ++ show ann_ty
checkVal (VRoll wval ann_ty)
    | TRecurs ty <- ann_ty = do
        ty' <- checkVal wval
        substTop ann_ty ty =? ty'
        return ann_ty
    | otherwise = fail $ "expected recursive type, but got " ++ show ann_ty
checkVal (VUnroll wval) = do
    ty <- checkVal wval
    case ty of
        TRecurs ty' -> return $ substTop ty ty'
        _           -> fail $ "expected recursive type, but got " ++ show ty
checkVal VNonsense = return TNonsense
checkVal (VPtr i) = do
    Just sty <- use $ talRegFile . rfStackTy
    checkStackLength (i + 1) sty
    return $ TPtr sty

checkInstr :: Instr -> StateT TalState IO ()
checkInstr (IAop _ rd rs v) = do
    checkReg rs >>=? TInt
    checkVal v >>=? TInt
    talRegFile . rfRegTy %= M.insert rd TInt
checkInstr (IBop _ r v) = do
    checkReg r >>=? TInt
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfTy' -> do
            rfty <- use talRegFile
            rfty <=? rfTy'
        _ -> fail "expected register file type"
checkInstr (ICall ty v) = do
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfty' -> do
            rfty <- use talRegFile
            rfty <=? rfty'
            talRegFile . rfRegTy %= M.insert RVReg ty
        _ -> fail "expected register file type"
checkInstr (ILoad rd rs i) = do
    guard $ i >= 0
    rsTy <- checkReg rs
    case rsTy of
        TRow rty -> talRegFile . rfRegTy %= M.insert rd (rty ^?! ix i)
        _        -> fail $ "expected row type, but got " ++ show rsTy
checkInstr (IMalloc rd tys) =
    talRegFile . rfRegTy %= M.insert rd (TRow $ foldr RSeq REmpty tys)
checkInstr (IMove rd v) = do
    vTy <- checkVal v
    talRegFile . rfRegTy %= M.insert rd vTy
checkInstr (IStore rd i rs) = do
    guard $ i >= 0
    rdTy <- checkReg rd
    case rdTy of
        TRow rty -> do
            checkReg rs >>=? rty ^?! ix i
            talRegFile . rfRegTy %= M.insert rd rdTy
        _ -> fail $ "expected row type, but got " ++ show rdTy
checkInstr (IUnpack rd v) = do
    vTy <- checkVal v
    case vTy of
        TExists ty -> do
            talRegFile . rfRegTy %= M.insert rd ty
            talQuants %= (() :)
        _ -> fail $ "expected existential type, but got " ++ show vTy
checkInstr (ISalloc n) =
    talRegFile . rfStackTy %= \case
        Nothing -> Just $ foldr SCons SNil (replicate n TNonsense)
        Just s -> Just $ foldr SCons s (replicate n TNonsense)
checkInstr (ISfree n) = do
    Just sty <- use $ talRegFile . rfStackTy
    checkStackLength n sty
    talRegFile . rfStackTy ?= popN n sty
  where
    popN :: Int -> StackTy -> StackTy
    popN m s | m <= 0 = s
    popN m (SCons _ xs) = popN (m + 1) xs
    popN _ _ = error "stack underflow"
checkInstr (ISload rd sp i) | SPReg <- sp = do
    guard $ i >= 0
    Just sty <- use $ talRegFile . rfStackTy
    checkStackLength (i + 1) sty
    talRegFile . rfRegTy %= M.insert rd (sty ^?! ix i)
checkInstr (ISload rd rs i) = do
    guard $ i >= 0
    Just sty@(SComp _ sty2) <- use $ talRegFile . rfStackTy
    checkStackLength (i + 1) sty
    TPtr sty2' <- checkReg rs
    sty2 =? sty2'
    talRegFile . rfRegTy %= M.insert rd (sty2 ^?! ix i)
checkInstr (ISstore sp i rs) | SPReg <- sp = do
    guard $ i >= 0
    Just sty <- use $ talRegFile . rfStackTy
    checkStackLength (i + 1) sty
    tyi <- checkReg rs
    talRegFile . rfStackTy ?= (sty & ix i .~ tyi)
checkInstr (ISstore rd i rs) = do
    guard $ i >= 0
    TPtr sty2' <- checkReg rd
    checkStackLength (i + 1) sty2'
    Just (SComp sty1 sty2) <- use $ talRegFile .rfStackTy
    sty2 =? sty2'
    tyi <- checkReg rs
    let sty3 = sty2 & ix i .~ tyi
    talRegFile . rfStackTy ?= SComp sty1 sty3
    talRegFile . rfRegTy %= M.insert rd (TPtr sty3)

checkStackLength :: MonadFail m => Int -> StackTy -> m ()
checkStackLength n _ | n <= 0 = return ()
checkStackLength n (SCons _ xs) = checkStackLength (n - 1) xs
checkStackLength _ _ = fail "stack underflow"

checkInstrs :: Instrs -> StateT TalState IO ()
checkInstrs (ISeq i is) = checkInstr i >> checkInstrs is
checkInstrs (IJump v) = do
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfTy' -> do
            rfty <- use talRegFile
            rfty <=? rfTy'
        _ -> fail "expected register file type"
checkInstrs (IHalt ty) = do
    checkReg RVReg >>=? ty
    talRegFile . rfRegTy %= M.insert RVReg ty

checkHeap :: (Name, Heap) -> StateT TalState IO ()
checkHeap (name, HGlobal wval) = do
    ty <- checkVal wval
    talHeaps %= M.insert name ty
checkHeap (_, HCode qnts rfty instrs) = do
    talQuants .= qnts
    talRegFile .= rfty
    checkInstrs instrs
    refresh
checkHeap (name, HStruct ws) = do
    wTys <- mapM checkVal ws
    talHeaps %= M.insert name (TRow $ foldr RSeq REmpty wTys)
checkHeap (name, HExtern ty) = talHeaps %= M.insert name ty
checkHeap (name, HTypeAlias ty) = talHeaps %= M.insert name ty

initHeapsTy :: Heaps -> IO HeapsTy
initHeapsTy heaps = do
    let htys = [ (name, TRegFile qnts rfty) | (name, HCode qnts rfty _) <- M.toList heaps ]
    return $ M.fromList htys

checkProgram :: Program -> IO ()
checkProgram (heaps, instrs) = do
    hsty <- initHeapsTy heaps
    let initState = TalState hsty emptyRegFileTy []
    void $ runStateT (mapM_ checkHeap (M.toList heaps) >> checkInstrs instrs) initState
