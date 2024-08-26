module Tal.Instruction
    ( Instruction
    , Reg
    , Imm
    , Addr
    , zr, ra, sp, rv
    , a0, a1, a2, a3
    , r0, r1, r2, r3, r4, r5, r6, r7
    , createHaltInst
    , createAddInst
    , createSubInst
    , createMulInst
    , createDivInst
    , createBeqInst
    , createBneInst
    , createBltInst
    , createBgtInst
    , createBleInst
    , createBgeInst
    , createCallInst
    , createJumpInst
    , createJumpRInst
    , createLoadInst
    , createLoadiInst
    , createMallocInst
    , createStoreInst
    , createUnpackInst
    , createSallocInst
    , createSfreeInst
    , createSloadInst
    , createSstoreInst
    , createIntegerData
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Tal.Context

type Instruction = CInt

type Reg = CInt

type Imm = CInt

type Addr = CInt

zr, ra, sp, rv :: Reg
zr = 0
ra = 1
sp = 2
rv = 3

a0, a1, a2, a3 :: Reg
a0 = 4
a1 = 5
a2 = 6
a3 = 7

r0, r1, r2, r3, r4, r5, r6, r7 :: Reg
r0 = 8
r1 = 9
r2 = 10
r3 = 11
r4 = 12
r5 = 13
r6 = 14
r7 = 15

foreign import ccall unsafe "HaltInst" createHaltInst :: Ptr Context -> IO ()

foreign import ccall unsafe "AddInst" createAddInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "SubInst" createSubInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "MulInst" createMulInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "DivInst" createDivInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "BeqInst" createBeqInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "BneInst" createBneInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "BltInst" createBltInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "BgtInst" createBgtInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "BleInst" createBleInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "BgeInst" createBgeInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CallInst" createCallInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "JumpInst" createJumpInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "JumpRInst" createJumpRInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "LoadInst" createLoadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "LoadiInst" createLoadiInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "MallocInst" createMallocInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "StoreInst" createStoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "UnpackInst" createUnpackInst :: Ptr Context -> Reg -> Reg -> IO ()

foreign import ccall unsafe "SallocInst" createSallocInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "SfreeInst" createSfreeInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "SloadInst" createSloadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "SstoreInst" createSstoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "IntegerData" createIntegerData :: Ptr Context -> Imm -> IO ()
