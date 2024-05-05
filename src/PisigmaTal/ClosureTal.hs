module PisigmaTal.ClosureTal where

import Control.Lens.Combinators
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.List                qualified as L
import Data.Map.Strict          qualified as M
import PisigmaTal.Closure       qualified as C
import PisigmaTal.Id
import PisigmaTal.Primitive
import Prelude                  hiding (exp)
import Tal.Builder
import Tal.Constant
import Tal.Constructors
import Tal.Syntax               qualified as T

newtype UserContext = UserContext
    { enums :: M.Map Id [C.Label]
    }

type TalBuilder = TalBuilderT (ReaderT UserContext IO)

getEnumLabels :: Id -> TalBuilder [C.Label]
getEnumLabels x = lift $
    asks (M.lookup x . enums) >>= \case
        Just ls -> return ls
        Nothing -> fail "enum not found"

allocTalTy :: C.Ty -> TalBuilder T.Ty
allocTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF x -> do
        tv <- getTyVar (idInt x)
        return $ T.TVar tv
    C.TFunF tys _ -> T.TRegFile . mkRegFileTy <$> sequence tys
    C.TExistsF _ ty -> T.TExists <$> ty
    C.TRecursF _ ty -> T.TRecurs <$> ty
    C.TRowF row -> T.TRow <$> allocTalRowTy row
    C.TNameF x -> T.TAlias <$> lookupName (idInt x)

allocTalRowTy :: C.RowTy -> TalBuilder T.RowTy
allocTalRowTy = cata $ \case
    C.REmptyF -> return T.REmpty
    C.RVarF x -> do
        tv <- getTyVar (idInt x)
        return $ T.RVar tv
    C.RSeqF ty rest -> T.RSeq <$> allocTalTy ty <*> rest

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

allocTalVal :: C.Val -> TalBuilder T.SmallVal
allocTalVal (C.VLit l) = return $ T.VWord $ closureTalLit l
allocTalVal (C.VVar x) = do
    reg <- findReg $ idInt $ fst x
    whenUseCountZero (idInt (fst x)) $ freeReg reg
    return $ T.VReg reg
allocTalVal (C.VFun f) = do
    reg <- findReg $ idInt $ fst f
    whenUseCountZero (idInt (fst f)) $ freeReg reg
    return $ T.VReg reg
allocTalVal (C.VLabel c l) = do
    labs <- getEnumLabels c
    i <- case L.elemIndex l labs of
        Just i  -> return i
        Nothing -> fail "label not found"
    return $ T.VWord $ T.VInt i
allocTalVal (C.VPack t1 v t2) =
    T.VPack <$> allocTalTy t1 <*> allocTalVal v <*> allocTalTy t2
allocTalVal (C.VRoll v t) = T.VRoll <$> allocTalVal v <*> allocTalTy t
allocTalVal (C.VUnroll v) = T.VUnroll <$> allocTalVal v
allocTalVal (C.VAnnot v _) = allocTalVal v

mapPrimop :: PrimOp -> T.Aop
mapPrimop = \case Add -> T.Add; Sub -> T.Sub; Mul -> T.Mul; Div -> T.Div

allocTalExp :: C.Exp -> TalBuilder T.Instrs
allocTalExp (C.ELet (C.BVal (x, ty) val) exp) = do
    ty' <- allocTalTy ty
    reg <- buildMove =<< allocTalVal val
    withExtReg (idInt x) reg ty' $ allocTalExp exp
allocTalExp (C.ELet (C.BCall (x, ty) var vals) exp) = do
    ty' <- allocTalTy ty
    val' <- allocTalVal $ C.VVar var
    vals' <- mapM allocTalVal vals
    argRegs <- mapM buildMove vals'
    tmpRegs <- getInUseRegs
    mapM_ buildPush tmpRegs
    mapM_ buildPush argRegs
    extInstr $ T.ICall val'
    mapM_ buildPop tmpRegs
    withExtReg (idInt x) RVReg ty' $ allocTalExp exp
allocTalExp (C.ELet (C.BOpCall (x, ty) prim _ vals) exp) = do
    ty' <- allocTalTy ty
    reg1 <- buildMove =<< allocTalVal (head vals)
    free <- isFreeReg reg1
    reg <- if free then setRegInUse reg1 >> return reg1 else freshReg
    val2 <- allocTalVal (vals !! 1)
    extInstr $ T.IAop (mapPrimop prim) reg reg1 val2
    withExtReg (idInt x) reg ty' $ allocTalExp exp
allocTalExp (C.ELet (C.BProj (x, ty) val idx) exp) = do
    ty' <- allocTalTy ty
    reg <- buildMove =<< allocTalVal val
    extInstr $ T.ILoad reg reg (pred (fromEnum idx))
    withExtReg (idInt x) reg ty' $ allocTalExp exp
allocTalExp (C.ELet (C.BUnpack tv (x, ty) val) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    val' <- allocTalVal val
    extInstr $ T.IUnpack reg val'
    withExtReg (idInt x) reg ty' $ withExtTyVarScope (idInt tv) $ allocTalExp exp
allocTalExp (C.ELet (C.BMalloc (x, ty) tys) exp) = do
    reg <- freshReg
    ty' <- allocTalTy ty
    tys' <- mapM allocTalTy tys
    extInstr $ T.IMalloc reg tys'
    withExtReg (idInt x) reg ty' $ allocTalExp exp
allocTalExp (C.ELet (C.BUpdate (x, ty) var idx val) exp) = do
    ty' <- allocTalTy ty
    reg <- buildMove =<< allocTalVal (C.VVar var)
    reg' <- buildMove =<< allocTalVal val
    extInstr $ T.IStore reg (pred (fromEnum idx)) reg'
    withExtReg (idInt x) reg ty' $ allocTalExp exp
allocTalExp (C.ECase con val cases) = do
    reg <- buildMove =<< allocTalVal val
    rfty <- view regFileTy
    labs <- getEnumLabels con
    heaps <- forM cases $ \(lab, exp) -> do
        instrs <- allocTalExp exp
        return (lab, T.HCode [] rfty instrs)
    heaps' <- sequence [ (,heap) <$> freshName l1 | l1 <- labs, (l2, heap) <- heaps, l1 == l2 ]
    extendHeaps heaps'
    let allocTalCases :: [T.Label] -> TalBuilder T.Instrs
        allocTalCases [] = buildInstrs $ T.IHalt T.TNonsense -- tmp: exception or default
        allocTalCases (lab : rest) = do
            extInstr $ T.IBop T.Bz reg (T.VWord (T.VLabel lab))
            extInstr $ T.IAop T.Add reg reg (T.VWord (T.VInt 1))
            allocTalCases rest
    allocTalCases (map fst heaps')
allocTalExp (C.EReturn val) = do
    val' <- allocTalVal val
    ty <- allocTalTy (C.typeof val)
    extInstr $ T.IMove RVReg val'
    buildInstrs $ T.IHalt ty
allocTalExp (C.EAnnot exp _) = allocTalExp exp
