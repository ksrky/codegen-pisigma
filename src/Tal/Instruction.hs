module Tal.Instruction
    ( Instruction
    , Reg
    , Imm
    , Addr
    , zr, ra, sp, rv, a0, r0
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
    , createJmpInst
    , createLoadInst
    , createLoadiInst
    , createMallocInst
    , createStoreInst
    , createUnpackInst
    , createSallocInst
    , createSfreeInst
    , createSloadInst
    , createSstoreInst
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Tal.Context

type Instruction = CInt

type Reg = CInt

type Imm = CInt

type Addr = CInt

foreign import ccall unsafe "ZR" zr :: Reg

foreign import ccall unsafe "RA" ra :: Reg

foreign import ccall unsafe "SP" sp :: Reg

foreign import ccall unsafe "rv" rv :: Reg

foreign import ccall unsafe "a0" a0 :: Reg

foreign import ccall unsafe "r0" r0 :: Reg

foreign import ccall unsafe "HaltInst" createHaltInst :: Ptr Context -> IO Instruction

foreign import ccall unsafe "AddInst" createAddInst :: Ptr Context -> Reg -> Reg -> Reg -> IO Instruction

foreign import ccall unsafe "SubInst" createSubInst :: Ptr Context -> Reg -> Reg -> Reg -> IO Instruction

foreign import ccall unsafe "MulInst" createMulInst :: Ptr Context -> Reg -> Reg -> Reg -> IO Instruction

foreign import ccall unsafe "DivInst" createDivInst :: Ptr Context -> Reg -> Reg -> Reg -> IO Instruction

foreign import ccall unsafe "BeqInst" createBeqInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "BneInst" createBneInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "BltInst" createBltInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "BgtInst" createBgtInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "BleInst" createBleInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "BgeInst" createBgeInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "CallInst" createCallInst :: Ptr Context -> Addr -> IO Instruction

foreign import ccall unsafe "JumpInst" createJmpInst :: Ptr Context -> Addr -> IO Instruction

foreign import ccall unsafe "LoadInst" createLoadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "LoadiInst" createLoadiInst :: Ptr Context -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "MallocInst" createMallocInst :: Ptr Context -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "StoreInst" createStoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "UnpackInst" createUnpackInst :: Ptr Context -> Reg -> Reg -> IO Instruction

foreign import ccall unsafe "SallocInst" createSallocInst :: Ptr Context -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "SfreeInst" createSfreeInst :: Ptr Context -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "SloadInst" createSloadInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction

foreign import ccall unsafe "SstoreInst" createSstoreInst :: Ptr Context -> Reg -> Reg -> Imm -> IO Instruction
