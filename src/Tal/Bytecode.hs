module Tal.Bytecode (runInstrs) where

import Tal.State
import Tal.Syntax

runInstrs :: (MonadTalState m, MonadFail m) => Instrs -> m ()
runInstrs (ISeq (IBop bop rd v) rest) = do
    VInt i <- fromReg rd
    if bopFun bop i then do
        VLabel l <- wordize v
        HCode _ ins <- getHeap l
        runInstrs ins
    else runInstrs rest
runInstrs (ISeq ins rest) = do
    runInstr ins
    runInstrs rest
runInstrs (IJump _) = undefined
runInstrs (IHalt _) = undefined

runInstr :: (MonadTalState m, MonadFail m) => Instr -> m ()
runInstr (IAop aop rd rs v) = do
    w1 <- fromReg rs
    w2 <- wordize v
    extendRegFile rd (aopFun aop w1 w2)
runInstr IBop{} = error "unreachable"
runInstr (ICall rd v) = undefined
runInstr (ILoad rd rs i) = do
    VLabel l <- fromReg rs
    HStruct ws <- getHeap l
    extendRegFile rd (ws !! i)
runInstr (IMalloc rd tys) = do
    -- lab <- newLabel "mal"
    undefined
runInstr (IMove rd rs) = undefined
runInstr (IStore rd rs i) = undefined
runInstr _                  = undefined

aopFun :: Num a => Aop -> a -> a -> a
aopFun Add = (+)
aopFun Sub = (-)
aopFun Mul = (*)

bopFun :: (Ord a, Num a) => Bop -> a -> Bool
bopFun Bz  = (== 0)
bopFun Bnz = (/= 0)
bopFun Bgt = (> 0)
bopFun Blt = (< 0)
