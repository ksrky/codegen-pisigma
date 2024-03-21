module Tal.Bytecode (runInstrs) where

import Tal.Constant
import Tal.State
import Tal.Syntax

runInstrs :: (MonadTalState m, MonadFail m) => Instrs -> m ()
runInstrs (ISeq ins rest) = do
    f <- runInstr ins
    runInstrs $ f rest
runInstrs (IJump v) = do
    VLabel l <- wordize v
    HCode _ ins <- getHeap l
    runInstrs ins
runInstrs (IHalt _) = do
    -- tmp: return result or set exit code
    _ret <- readReg returnReg
    return ()

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

aopFun :: Num a => Aop -> a -> a -> a
aopFun Add = (+)
aopFun Sub = (-)
aopFun Mul = (*)

bopFun :: (Ord a, Num a) => Bop -> a -> Bool
bopFun Bz  = (== 0)
bopFun Bnz = (/= 0)
bopFun Bgt = (> 0)
bopFun Blt = (< 0)
