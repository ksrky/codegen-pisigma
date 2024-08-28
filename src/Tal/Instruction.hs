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

foreign import ccall unsafe "CreateHaltInst" createHaltInst :: Ptr Context -> IO ()

foreign import ccall unsafe "CreateAddInst" createAddInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "CreateSubInst" createSubInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "CreateMulInst" createMulInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "CreateDivInst" createDivInst :: Ptr Context -> Reg -> Reg -> Reg -> IO ()

foreign import ccall unsafe "CreateBeqInst" createBeqInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateBneInst" createBneInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateBltInst" createBltInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateBgtInst" createBgtInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateBleInst" createBleInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateBgeInst" createBgeInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateCallInst" createCallInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "CreateJumpInst" createJumpInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "CreateJumpRInst" createJumpRInst :: Ptr Context -> Addr -> IO ()

foreign import ccall unsafe "CreateLoadInst" createLoadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateLoadiInst" createLoadiInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateMallocInst" createMallocInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateStoreInst" createStoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateUnpackInst" createUnpackInst :: Ptr Context -> Reg -> Reg -> IO ()

foreign import ccall unsafe "CreateSallocInst" createSallocInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateSfreeInst" createSfreeInst :: Ptr Context -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateSloadInst" createSloadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateSstoreInst" createSstoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO ()

foreign import ccall unsafe "CreateIntegerData" createIntegerData :: Ptr Context -> Imm -> IO ()
