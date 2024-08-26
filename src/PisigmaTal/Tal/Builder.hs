module PisigmaTal.Tal.Builder
    ( buildProgram
    , buildNewProgram
    ) where

import Foreign.C.Types
import Foreign.Ptr
import PisigmaTal.Tal
import PisigmaTal.Tal.Constant
import Tal.Context
import Tal.Instruction         qualified as Tal

type LabelDict = [(Label, CInt)]

regId :: Reg -> Tal.Reg
regId ZRReg          = Tal.zr
regId RAReg          = Tal.ra
regId SPReg          = Tal.sp
regId RVReg          = Tal.rv
regId A0Reg          = Tal.a0
regId A1Reg          = Tal.a1
regId A2Reg          = Tal.a2
regId A3Reg          = Tal.a3
regId (GeneralReg i) = 8 + fromIntegral i
regId _              = error "invalid register"

buildVal :: LabelDict -> Val a -> CInt
buildVal _ (VReg r)      = regId r
buildVal d (VWord v)     = buildVal d v
buildVal d (VLabel l)    = case lookup l d of
    Just i  -> i
    Nothing -> error "label not found"
buildVal _ (VInt i)      = fromIntegral i
buildVal _ (VJunk _)     = 0
buildVal d (VPack _ v _) = buildVal d v
buildVal d (VRoll v _)   = buildVal d v
buildVal d (VUnroll v)   = buildVal d v
buildVal _ VNonsense     = 0
buildVal _ (VPtr i)      = fromIntegral i

buildInstr :: LabelDict -> Ptr Context -> Instr -> IO ()
buildInstr d ctx (IAop Add rd rs v) = Tal.createAddInst ctx (regId rd) (regId rs) (buildVal d v)
buildInstr d ctx (IAop Sub rd rs v) = Tal.createSubInst ctx (regId rd) (regId rs) (buildVal d v)
buildInstr d ctx (IAop Mul rd rs v) = Tal.createMulInst ctx (regId rd) (regId rs) (buildVal d v)
buildInstr d ctx (IAop Div rd rs v) = Tal.createDivInst ctx (regId rd) (regId rs) (buildVal d v)
buildInstr d ctx (IBop Bz r v)      = Tal.createBeqInst ctx (regId r) Tal.zr (buildVal d v)
buildInstr d ctx (IBop Bnz r v)     = Tal.createBneInst ctx (regId r) Tal.zr (buildVal d v)
buildInstr d ctx (IBop Bgt r v)     = Tal.createBgtInst ctx (regId r) Tal.zr (buildVal d v)
buildInstr d ctx (IBop Blt r v)     = Tal.createBltInst ctx (regId r) Tal.zr (buildVal d v)
buildInstr d ctx (ICall _ v)       = Tal.createCallInst ctx (buildVal d v)
buildInstr _ ctx (ILoad rd rs i)   = Tal.createLoadInst ctx (regId rd) (regId rs) (fromIntegral i)
buildInstr _ ctx (IMalloc rd tys)  = Tal.createMallocInst ctx (regId rd) (fromIntegral $ length tys)
buildInstr d ctx (IMove rd v)      = Tal.createLoadiInst ctx (regId rd) (buildVal d v)
buildInstr _ ctx (IStore rd i rs)  = Tal.createStoreInst ctx (regId rs) (regId rd) (fromIntegral i)
buildInstr _ ctx (IUnpack rd)      = Tal.createUnpackInst ctx (regId rd) (regId rd)
buildInstr _ ctx (ISalloc n)       = Tal.createSallocInst ctx Tal.sp (fromIntegral n)
buildInstr _ ctx (ISfree n)        = Tal.createSfreeInst ctx Tal.sp (fromIntegral n)
buildInstr _ ctx (ISload rd rs i)  = Tal.createSloadInst ctx (regId rd) (regId rs) (fromIntegral i)
buildInstr _ ctx (ISstore rd i rs) = Tal.createSstoreInst ctx (regId rs) (regId rd) (fromIntegral i)

buildInstrs :: LabelDict -> Ptr Context -> Instrs -> IO ()
buildInstrs d ctx (ISeq i is) = buildInstr d ctx i >> buildInstrs d ctx is
buildInstrs d ctx (IJump v)   = Tal.createJumpInst ctx (buildVal d v)
buildInstrs _ ctx (IHalt _)   = Tal.createHaltInst ctx

buildHeaps :: LabelDict -> Ptr Context -> Heaps -> IO LabelDict
buildHeaps dict _ [] = return dict
buildHeaps dict ctx (HGlobal l v : heaps)    = do
    addr <- getDataEnd ctx
    Tal.createIntegerData ctx (buildVal dict v)
    buildHeaps ((l, addr) : dict) ctx heaps
buildHeaps dict ctx (HCode l _ _ is : heaps) = do
    addr <- getProgramEnd ctx
    let dict' = (l, addr) : dict
    buildInstrs dict ctx is
    buildHeaps dict' ctx heaps
buildHeaps dict ctx (_ : heaps) = buildHeaps dict ctx heaps

buildProgram :: Ptr Context -> Program -> IO ()
buildProgram ctx (heaps, is) = do
    dict <- buildHeaps [] ctx heaps
    buildInstrs dict ctx is

buildNewProgram :: Program -> IO (Ptr Context)
buildNewProgram prog = do
    ctx <- createContext
    buildProgram ctx prog
    return ctx
