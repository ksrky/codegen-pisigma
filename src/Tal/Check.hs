{-# LANGUAGE TemplateHaskell #-}

module Tal.Check (checkProgram) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Tal.Constant
import Tal.Syntax

data TalState = TalState
    { _talHeaps   :: HeapsTy
    , _talRegFile :: RegFileTy
    , _talStack   :: StackTy
    , _talQuants  :: Quants
    }

makeLenses ''TalState

refresh :: StateT TalState IO ()
refresh = do
    talRegFile .= M.empty
    talStack .= SNil
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


instance TyEquiv a => TyEquiv [a] where
    [] =? []             = return ()
    (x : xs) =? (y : ys) = x =? y >> xs =? ys
    _ =? _               = fail "list length mismatch"

instance TyEquiv a => TySubrel [a] where
    [] <=? _              = return ()
    (x : xs) <=? (y : ys) = x =? y >> xs <=? ys
    _ <=? []              = fail "not subtype"

instance TyEquiv Ty where
    TInt =? TInt = return ()
    TVar x =? TVar y | x == y = return ()
    TRegFile qs1 rf1 st1 =? TRegFile qs2 rf2 st2 | length qs1 == length qs2 = rf1 =? rf2 >> st1 =? st2
    TExists t1 =? TExists t2 = t1 =? t2
    TRecurs t1 =? TRecurs t2 = t1 =? t2
    TRow r1 =? TRow r2 = r1 =? r2
    TNonsense =? TNonsense = return ()
    TPtr sty1 =? TPtr sty2 = sty1 =? sty2
    TAlias x =? TAlias y | x == y = return () -- tmp
    t1 =? t2 = fail $ "type mismatch. expected: " ++ show t1 ++ ", got: " ++ show t2

instance TySubrel Ty where
    TRegFile _ rf1 st1 <=? TRegFile _ rf2 st2 = rf1 <=? rf2 >> st1 =? st2 -- TODO: is it OK to discard quantifiers?
    ty1 <=? ty2                               = ty1 =? ty2

instance TyEquiv RowTy where
    REmpty =? REmpty = return ()
    RVar x =? RVar y | x == y = return ()
    RSeq t1 r1 =? RSeq t2 r2 = t1 =? t2 >> r1 =? r2
    r1 =? r2 = fail $ "type mismatch. expected: " ++ show r1 ++ ", got: " ++ show r2

instance TyEquiv RegFileTy where
    rf1 =? rf2 = M.elems rf1 =? M.elems rf2

instance TySubrel RegFileTy where
    r1 <=? r2 = M.elems r1 <=? M.elems r2

instance TyEquiv StackTy where
    SNil =? SNil = return ()
    SVar x =? SVar y | x == y = return ()
    SCons t1 s1 =? SCons t2 s2 = t1 =? t2 >> s1 =? s2
    s1 =? s2 = fail $ "stack type mismatch. expected: " ++ show s1 ++ ", got: " ++ show s2

checkReg :: Reg -> StateT TalState IO Ty
checkReg reg =  do
    regFile <- use talRegFile
    case M.lookup reg regFile of
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
checkVal (VPtr _) = do
    sty <- use talStack
    return $ TPtr sty -- tmp

checkInstr :: Instr -> StateT TalState IO ()
checkInstr (IAop _ rd rs v) = do
    checkReg rs >>=? TInt
    checkVal v >>=? TInt
    talRegFile %= M.insert rd TInt
checkInstr (IBop _ r v) = do
    checkReg r >>=? TInt
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfTy' sty' -> do
            rfty <- use talRegFile
            rfty <=? rfTy'
            sty <- use talStack
            sty =? sty'
        _ -> fail "expected register file type"
checkInstr (ICall v) = do
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfTy' sty' -> do
            rfty <- use talRegFile
            rfty <=? rfTy'
            sty <- use talStack
            sty =? sty'
        _ -> fail "expected register file type"
checkInstr (ILoad rd rs i) = do
    rsTy <- checkReg rs
    case rsTy of
        TRow rty -> talRegFile %= M.insert rd (rty ^?! ix i)
        _        -> fail "expected pointer type"
checkInstr (IMalloc rd tys) =
    talRegFile %= M.insert rd (TRow $ foldr RSeq REmpty tys)
checkInstr (IMove rd v) = do
    vTy <- checkVal v
    talRegFile %= M.insert rd vTy
checkInstr (IStore rd i rs) = do
    rdTy <- checkReg rd
    case rdTy of
        TRow rty -> do
            checkReg rs >>=? rty ^?! ix i
            talRegFile %= M.insert rd rdTy
        _ -> fail "expected pointer type"
checkInstr (IUnpack rd v) = do
    vTy <- checkVal v
    case vTy of
        TExists ty -> do
            talRegFile %= M.insert rd ty
            talQuants %= (() :)
        _ -> fail "expected row type"
checkInstr (ISalloc n) =
    talStack %= (\s -> foldr SCons s (replicate n TNonsense))
checkInstr (ISfree n) = do
    checkStackLength n =<< use talStack
    talStack %= popN n
  where
    popN :: Int -> StackTy -> StackTy
    popN m s | m <= 0 = s
    popN m (SCons _ xs) = popN (m + 1) xs
    popN _ _ = error "stack underflow"
checkInstr (ISload rd sp i) | SPReg <- sp = do
    sty <- use talStack
    checkStackLength (i + 1)sty
    talRegFile %= M.insert rd (sty ^?! ix i)
checkInstr (ISload rd rs i) = do
    checkStackLength (i + 1) =<< use talStack
    TPtr sty <- checkReg rs
    talRegFile %= M.insert rd (sty ^?! ix i)
checkInstr (ISstore sp i rs) | SPReg <- sp = do
    checkStackLength (i + 1) =<< use talStack
    tyi <- checkReg rs
    talStack %= (ix i .~ tyi)
checkInstr (ISstore rd i rs) = do
    TPtr sty <- checkReg rd
    checkStackLength (i + 1) sty
    tyi <- checkReg rs
    talStack %= (ix i .~ tyi) -- tmp
    talRegFile %= M.insert rd (TPtr (sty & ix i .~ tyi))

checkStackLength :: MonadFail m => Int -> StackTy -> m ()
checkStackLength n _ | n <= 0 = return ()
checkStackLength n (SCons _ xs) = checkStackLength (n - 1) xs
checkStackLength _ _ = fail "stack underflow"

checkInstrs :: Instrs -> StateT TalState IO ()
checkInstrs (ISeq i is) = checkInstr i >> checkInstrs is
checkInstrs (IJump v) = do
    vTy <- checkVal v
    case vTy of
        TRegFile [] rfTy' sty' -> do
            rfty <- use talRegFile
            rfty <=? rfTy'
            sty <- use talStack
            sty =? sty'
        _ -> fail "expected register file type"
checkInstrs (IHalt ty) = checkReg RVReg >>=? ty

checkHeap :: (Name, Heap) -> StateT TalState IO ()
checkHeap (name, HGlobal wval) = do
    ty <- checkVal wval
    talHeaps %= M.insert name ty
checkHeap (_, HCode qnts rfty sty instrs) = do
    talQuants .= qnts
    talRegFile .= rfty
    talStack .= sty
    checkInstrs instrs
    refresh
checkHeap (name, HStruct ws) = do
    wTys <- mapM checkVal ws
    talHeaps %= M.insert name (TRow $ foldr RSeq REmpty wTys)
checkHeap (name, HExtern ty) = talHeaps %= M.insert name ty
checkHeap (name, HTypeAlias ty) = talHeaps %= M.insert name ty

initHeapsTy :: Heaps -> IO HeapsTy
initHeapsTy heaps = do
    let htys = [ (name, TRegFile qnts rfty sty) | (name, HCode qnts rfty sty _) <- M.toList heaps ]
    return $ M.fromList htys

checkProgram :: Program -> IO ()
checkProgram (heaps, instrs) = do
    hsty <- initHeapsTy heaps
    let initState = TalState hsty M.empty SNil []
    void $ runStateT (mapM_ checkHeap (M.toList heaps) >> checkInstrs instrs) initState
