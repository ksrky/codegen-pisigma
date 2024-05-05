{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.ClosureTal (closureTalProgram) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.Writer
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
    { _enums :: M.Map Id [C.Label]
    }
    deriving (Monoid)

instance Semigroup UserContext where
    UserContext {_enums = e1} <> UserContext {_enums = e2} =
        UserContext {_enums = e1 <> e2}

makeLenses ''UserContext

type TalBuilder = TalBuilderT (ReaderT UserContext IO)

getEnumLabels :: Id -> TalBuilder [C.Label]
getEnumLabels x = lift $
    views enums (M.lookup x) >>= \case
        Just ls -> return ls
        Nothing -> fail "enum not found"

closureTalTy :: MonadFail m => C.Ty -> TalBuilderT m T.Ty
closureTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF x -> do
        tv <- getTyVar (idInt x)
        return $ T.TVar tv
    C.TFunF tys _ -> T.TRegFile . mkRegFileTy <$> sequence tys
    C.TExistsF _ ty -> T.TExists <$> ty
    C.TRecursF _ ty -> T.TRecurs <$> ty
    C.TRowF row -> T.TRow <$> closureTalRowTy row
    C.TNameF x -> T.TAlias <$> lookupName (idInt x)

closureTalRowTy :: MonadFail m => C.RowTy -> TalBuilderT m T.RowTy
closureTalRowTy = cata $ \case
    C.REmptyF -> return T.REmpty
    C.RVarF x -> do
        tv <- getTyVar (idInt x)
        return $ T.RVar tv
    C.RSeqF ty rest -> T.RSeq <$> closureTalTy ty <*> rest

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

closureTalVal :: C.Val -> TalBuilder T.SmallVal
closureTalVal (C.VLit l) = return $ T.VWord $ closureTalLit l
closureTalVal (C.VVar x) = do
    reg <- findReg $ idInt $ fst x
    whenUseCountZero (idInt (fst x)) $ freeReg reg
    return $ T.VReg reg
closureTalVal (C.VFun f) = do
    name' <- lookupName $ idInt $ fst f
    return $ T.VWord $ T.VLabel name'
closureTalVal (C.VLabel c l) = do
    labs <- getEnumLabels c
    i <- case L.elemIndex l labs of
        Just i  -> return i
        Nothing -> fail "label not found"
    return $ T.VWord $ T.VInt i
closureTalVal (C.VPack t1 v t2) =
    T.VPack <$> closureTalTy t1 <*> closureTalVal v <*> closureTalTy t2
closureTalVal (C.VRoll v t) = T.VRoll <$> closureTalVal v <*> closureTalTy t
closureTalVal (C.VUnroll v) = T.VUnroll <$> closureTalVal v
closureTalVal (C.VAnnot v _) = closureTalVal v

mapPrimop :: PrimOp -> T.Aop
mapPrimop = \case Add -> T.Add; Sub -> T.Sub; Mul -> T.Mul; Div -> T.Div

closureTalExp :: C.Exp -> TalBuilder T.Instrs
closureTalExp (C.ELet (C.BVal (x, ty) val) exp) = do
    ty' <- closureTalTy ty
    reg <- buildMove =<< closureTalVal val
    withExtReg (idInt x) reg ty' $ closureTalExp exp
closureTalExp (C.ELet (C.BCall (x, ty) var vals) exp) = do
    ty' <- closureTalTy ty
    val' <- closureTalVal $ C.VVar var
    vals' <- mapM closureTalVal vals
    argRegs <- mapM buildMove vals'
    tmpRegs <- getInUseRegs
    mapM_ buildPush tmpRegs
    mapM_ buildPush argRegs
    extInstr $ T.ICall val'
    mapM_ buildPop tmpRegs
    withExtReg (idInt x) RVReg ty' $ closureTalExp exp
closureTalExp (C.ELet (C.BOpCall (x, ty) prim _ vals) exp) = do
    ty' <- closureTalTy ty
    reg1 <- buildMove =<< closureTalVal (head vals)
    free <- isFreeReg reg1
    reg <- if free then setRegInUse reg1 >> return reg1 else freshReg
    val2 <- closureTalVal (vals !! 1)
    extInstr $ T.IAop (mapPrimop prim) reg reg1 val2
    withExtReg (idInt x) reg ty' $ closureTalExp exp
closureTalExp (C.ELet (C.BProj (x, ty) val idx) exp) = do
    ty' <- closureTalTy ty
    reg <- buildMove =<< closureTalVal val
    extInstr $ T.ILoad reg reg (pred (fromEnum idx))
    withExtReg (idInt x) reg ty' $ closureTalExp exp
closureTalExp (C.ELet (C.BUnpack tv (x, ty) val) exp) = do
    reg <- freshReg
    ty' <- closureTalTy ty
    val' <- closureTalVal val
    extInstr $ T.IUnpack reg val'
    withExtReg (idInt x) reg ty' $ withExtTyVarScope (idInt tv) $ closureTalExp exp
closureTalExp (C.ELet (C.BMalloc (x, ty) tys) exp) = do
    reg <- freshReg
    ty' <- closureTalTy ty
    tys' <- mapM closureTalTy tys
    extInstr $ T.IMalloc reg tys'
    withExtReg (idInt x) reg ty' $ closureTalExp exp
closureTalExp (C.ELet (C.BUpdate (x, ty) var idx val) exp) = do
    ty' <- closureTalTy ty
    reg <- buildMove =<< closureTalVal (C.VVar var)
    reg' <- buildMove =<< closureTalVal val
    extInstr $ T.IStore reg (pred (fromEnum idx)) reg'
    withExtReg (idInt x) reg ty' $ closureTalExp exp
closureTalExp (C.ECase con val cases) = do
    reg <- buildMove =<< closureTalVal val
    rfty <- view regFileTy
    labs <- getEnumLabels con
    heaps <- forM cases $ \(lab, exp) -> do
        instrs <- closureTalExp exp
        return (lab, T.HCode [] rfty instrs)
    heaps' <- sequence [ (,heap) <$> freshName l1 | l1 <- labs, (l2, heap) <- heaps, l1 == l2 ]
    extendHeaps heaps'
    let closureTalCases :: [T.Label] -> TalBuilder T.Instrs
        closureTalCases [] = buildInstrs $ T.IHalt T.TNonsense -- tmp: exception or default
        closureTalCases (lab : rest) = do
            extInstr $ T.IBop T.Bz reg (T.VWord (T.VLabel lab))
            extInstr $ T.IAop T.Add reg reg (T.VWord (T.VInt 1))
            closureTalCases rest
    closureTalCases (map fst heaps')
closureTalExp (C.EReturn val) = do
    val' <- closureTalVal val
    ty <- closureTalTy (C.typeof val)
    extInstr $ T.IMove RVReg val'
    buildInstrs $ T.IHalt ty
closureTalExp (C.EAnnot exp _) = closureTalExp exp

closureTalDefn :: C.Defn -> TalBuilder ()
closureTalDefn (var, C.Code args body) = do
    name' <- lookupName $ idInt $ fst var
    rfty <- mkRegFileTy <$> mapM (closureTalTy . snd) args
    argRegs <- mapM (const freshReg) args
    let loads = map (\r -> T.ILoad r SPReg 0) argRegs
    instrs <- closureTalExp body
    extendHeap name' $ T.HCode [] rfty (foldl (flip T.ISeq) instrs loads)

closureTalDec :: C.Dec -> WriterT UserContext (TalBuilderT IO) ()
closureTalDec (C.DEnum con labs) = do
    scribe enums (M.singleton con labs)
    name' <- lift $ freshName (con ^. name)
    lift $ extendHeap name' $ T.HTypeAlias T.TInt
closureTalDec (C.DBind x ty) = lift $ do
    name' <- freshName (x ^. name)
    ty' <- closureTalTy ty
    extendHeap name' $ T.HExtern ty'

closureTalProgram :: C.Program -> IO T.Program
closureTalProgram (decs, (defns, exp)) = do
    ctx <- runTalBuilder initBuilderContext initBuilderState (execWriterT $ mapM closureTalDec decs)
    (`runReaderT` ctx) $
        runTalBuilder initBuilderContext initBuilderState $ do
            mapM_ (\((x, _), _) -> freshName $ x ^. name) defns
            mapM_ closureTalDefn defns
            instrs <- closureTalExp exp
            heaps <- use heapsState
            return (heaps, instrs)
