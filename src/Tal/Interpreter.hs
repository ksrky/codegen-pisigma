module Tal.Interpreter (runProgram) where

import Control.Lens.Operators
import Control.Monad.IO.Class
import Tal.Constant
import Tal.State
import Tal.Syntax

runProgram :: (MonadIO m, MonadFail m) => Uniq -> Program -> m Word
runProgram uniq (hs, rf, instrs) = do
    let st = defaultTalState
            & heaps .~ hs
            & regFile .~ rf
            & nextUniq .~ uniq
    ret <- evalTalState (runInstrs instrs >> readReg returnReg) st
    return $ liftMetaWord ret

runInstrs :: (MonadTalState m, MonadFail m) => Instrs -> m ()
runInstrs (ISeq ins rest) = do
    f <- runInstr ins
    runInstrs $ f rest
runInstrs (IJump v) = do
    VLabel l <- wordize v
    HCode _ ins <- getHeap l
    runInstrs ins
runInstrs (IHalt _) = return ()

runInstr :: (MonadTalState m, MonadFail m) => Instr -> m (Instrs -> Instrs)
runInstr (IAop aop rd rs v) = do
    w1 <- readReg rs
    w2 <- wordize v
    extendRegFile rd (aopFun aop w1 w2)
    return id
runInstr (IBop bop rd v) = do
    VInt i <- readReg rd
    if bopFun bop i then do
        VLabel l <- wordize v
        HCode _ ins <- getHeap l
        return $ const ins
    else return id
runInstr (ICall v) = do
    VLabel l <- wordize v
    HCode _ ins <- getHeap l
    return $ const ins
runInstr (ILoad rd rs i) = do
    VLabel l <- readReg rs
    HStruct ws <- getHeap l
    extendRegFile rd (ws !! i)
    return id
runInstr (IMalloc rd tys) = do
    lab <- freshName "mal"
    extendHeap (lab, HStruct $ map VJunk tys)
    extendRegFile rd (VLabel lab)
    return id
runInstr (IMove rd v) = do
    w <- wordize v
    extendRegFile rd w
    return id
runInstr (IStore rd i rs) = do
    VLabel l <- readReg rd
    HStruct ws <- getHeap l
    w <- readReg rs
    let ws' = take i ws ++ [w] ++ drop (i + 1) ws
    extendHeap (l, HStruct ws')
    return id
runInstr (IUnpack rd v) = do
    VPack _ w _ <- wordize v
    extendRegFile rd w
    return id
runInstr (ISalloc n) = do
    allocStack $ replicate n VNonsense
    return id
runInstr (ISfree n) = do
    freeStack n
    return id
runInstr (ISload rd sp i) | SpecialReg "sp" <- sp = do
    extendRegFile rd =<< readSlot Nothing i
    return id
runInstr (ISload rd rs i) = do
    VPtr ptr <- readReg rs
    extendRegFile rd =<< readSlot (Just ptr) i
    undefined
runInstr (ISstore sp i rs) | SpecialReg "sp" <- sp = do
    w <- readReg rs
    writeSlot Nothing i w
    return id
runInstr (ISstore rd i rs) = do
    VPtr ptr <- readReg rd
    w <- readReg rs
    writeSlot (Just ptr) i w
    undefined

aopFun :: Num a => Aop -> a -> a -> a
aopFun Add = (+)
aopFun Sub = (-)
aopFun Mul = (*)

bopFun :: (Ord a, Num a) => Bop -> a -> Bool
bopFun Bz  = (== 0)
bopFun Bnz = (/= 0)
bopFun Bgt = (> 0)
bopFun Blt = (< 0)
