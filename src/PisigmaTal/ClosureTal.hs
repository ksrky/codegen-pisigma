{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.ClosureTal (closureTalProgram) where

import Control.Lens.Combinators hiding (op)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
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

newtype UserState = UserState
    { _user_enums :: M.Map Id [C.Label]
    }
    deriving (Monoid)

instance Semigroup UserState where
    UserState e1 <> UserState e2 = UserState {_user_enums = e1 <> e2}

makeLenses ''UserState

emptyUserState :: UserState
emptyUserState = UserState M.empty

type TalBuilder = TalBuilderT (StateT UserState IO)

getEnumLabels :: Id -> TalBuilder [C.Label]
getEnumLabels x = lift $ lift $ do
    enums <- use user_enums
    case M.lookup x enums of
        Just ls -> return ls
        Nothing -> fail "enum not found"

freshName :: Monad m => Id -> TalBuilderT m T.Name
freshName x = newName (x ^. unique) (x ^. name)

freshNameFromString :: MonadIO m => String -> TalBuilderT m T.Name
freshNameFromString hint = freshName =<< newId hint

closureTalTy :: MonadFail m => C.Ty -> TalBuilderT m T.Ty
closureTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF x -> T.TVar <$> getTyVar (x ^. unique)
    C.TFunF tys _ -> T.TRegFile [] . mkArgRegFileTy <$> sequence tys
    C.TExistsF x ty -> T.TExists <$> withExtTyVarScope (x ^. unique) ty
    C.TRecursF x ty -> T.TRecurs <$> withExtTyVarScope (x ^. unique) ty
    C.TRowF row -> T.TRow <$> closureTalRowTy row
    C.TNameF x -> T.TAlias <$> lookupName (x ^. unique)

closureTalRowTy :: MonadFail m => C.RowTy -> TalBuilderT m T.RowTy
closureTalRowTy = cata $ \case
    C.REmptyF -> return T.REmpty
    C.RVarF x -> T.RVar <$> getTyVar (x ^. unique)
    C.RSeqF ty rest -> T.RSeq <$> closureTalTy ty <*> rest

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

closureTalVar :: C.Var -> TalBuilder T.Reg
closureTalVar (x, _) = do
    reg <- findReg $ x ^. unique
    decUseCount (x ^. unique)
    whenUseCountZero (x ^. unique) $ setRegFree reg
    return reg

closureTalVal :: C.Val -> TalBuilder T.SmallVal
closureTalVal (C.VLit l) = return $ T.VWord $ closureTalLit l
closureTalVal (C.VVar var) = T.VReg <$> closureTalVar var
closureTalVal (C.VFun f) = do
    name' <- lookupName $ fst f ^. unique
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

withExtReg :: C.Var -> T.Reg ->  TalBuilder a -> TalBuilder a
withExtReg (x, ty) reg cont = do
    ty' <- closureTalTy ty
    whenUseCountZero (x ^. unique) $ setRegFree reg
    withExtRegTable (x ^. unique) reg $ withExtRegFileTy reg ty' cont

withExtRegs :: [C.Var] -> [T.Reg] -> TalBuilder a -> TalBuilder a
withExtRegs [] [] cont = cont
withExtRegs (var : vars) (reg : regs) cont = withExtReg var reg $ withExtRegs vars regs cont
withExtRegs _ _ _ = error "impossible"

closureTalExp :: C.Exp -> TalBuilder T.Instrs
closureTalExp (C.ELet (C.BVal var val) exp) = do
    reg <- buildMove =<< closureTalVal val
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BCall var fun vals) exp) = do
    reg <- closureTalVar fun
    vals' <- mapM closureTalVal vals
    zipWithM_ buildMoveWithDst argumentRegs vals'
    tmp_regs <- getInUseRegs
    buildStores tmp_regs
    ret_ty <- closureTalTy (C.typeof var)
    extInstr $ T.ICall ret_ty (T.VReg reg)
    buildLoads tmp_regs
    withExtReg var RVReg $ closureTalExp exp
closureTalExp (C.ELet (C.BOpCall var (C.KnownOp f _) vals) exp) = do
    vals' <- mapM closureTalVal vals
    zipWithM_ buildMoveWithDst argumentRegs vals'
    tmp_regs <- getInUseRegs
    buildStores tmp_regs
    lab <- freshName f
    ret_ty <- closureTalTy (C.typeof var)
    extInstr $ T.ICall ret_ty (T.VWord (T.VLabel lab))
    buildLoads tmp_regs
    withExtReg var RVReg $ closureTalExp exp
closureTalExp (C.ELet (C.BOpCall var (C.PrimOp op _) vals) exp) | [val1, val2] <- vals = do
    reg <- freshReg
    buildMoveWithDst reg =<< closureTalVal val1
    val2' <- closureTalVal val2
    extInstr $ T.IAop (mapPrimop op) reg reg val2'
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet C.BOpCall{} _) = error "impossible" -- tmp
closureTalExp (C.ELet (C.BProj var val idx) exp) = do
    reg <- freshReg
    buildMoveWithDst reg =<< closureTalVal val
    extInstr $ T.ILoad reg reg (pred (fromEnum idx))
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BUnpack tv var val) exp) = do
    val' <- closureTalVal val
    reg <- freshReg
    extInstr $ T.IUnpack reg val'
    withExtTyVarScope (tv ^. unique) $ withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BMalloc var tys) exp) = do
    reg <- freshReg
    tys' <- mapM closureTalTy tys
    extInstr $ T.IMalloc reg tys'
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BUpdate var var' idx val) exp) = do
    reg' <- buildMove =<< closureTalVal val
    reg <- closureTalVar var'
    extInstr $ T.IStore reg (pred (fromEnum idx)) reg'
    setRegFree reg'
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ECase con val cases) = do
    reg <- buildMove =<< closureTalVal val
    labs <- getEnumLabels con
    heaps <- forM cases $ \(lab, exp) -> do
        qnts <- view quants
        rfty <- view regFileTy
        instrs <- closureTalExp exp
        return (lab, T.HCode qnts rfty instrs)
    heaps' <- sequence [ (,heap) <$> freshNameFromString l1 | l1 <- labs, (l2, heap) <- heaps, l1 == l2 ]
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
    buildMoveWithDst RVReg val'
    buildInstrs $ T.IHalt ty
closureTalExp (C.EAnnot exp _) = closureTalExp exp

closureTalDefn :: C.Defn -> TalBuilder ()
closureTalDefn (var, C.Code args body) = do
    when (length args >= length argumentRegs) $ fail "too many arguments" -- TODO: splilling
    name' <- lookupName (fst var ^. unique)
    arg_tys <- mapM (closureTalTy . snd) args
    tmp_regs <- mapM (const freshReg) args
    mapM_ setRegInUse tmp_regs
    zipWithM_ (\t a -> buildMoveWithDst t (T.VReg a)) tmp_regs argumentRegs
    instrs <- withExtRegs args tmp_regs $ closureTalExp body
    extendHeap name' $ T.HCode [] (mkArgRegFileTy arg_tys) instrs

closureTalDec :: C.Dec -> TalBuilder ()
closureTalDec (C.DEnum con labs) =  do
    lift $ lift $ user_enums %= M.insert con labs
    name' <- freshName con
    extendHeap name' (T.HTypeAlias T.TInt)
closureTalDec (C.DBind x ty) = do
    name' <- freshName x
    ty' <- closureTalTy ty
    extendHeap name' (T.HExtern ty')

closureTalProgram :: C.Program -> IO T.Program
closureTalProgram (decs, (defns, exp)) =
    (`evalStateT` emptyUserState) $
        runTalBuilderT initBuilderContext initBuilderState $ do
            mapM_ closureTalDec decs
            mapM_ (\((x, _), _) -> freshName x) defns
            mapM_ countUse defns
            mapM_ closureTalDefn defns
            countUse exp
            instrs <- closureTalExp exp
            heaps' <- use heapsState
            return (heaps', instrs)

class CountUse a where
    countUse :: a -> TalBuilder ()

instance CountUse C.Var where
    countUse (x, _) = incUseCount $ x ^. unique

instance CountUse C.Val where
    countUse = cata $ \case
        C.VVarF x -> countUse x
        v -> sequence_ v

instance CountUse C.Exp where
    countUse = cata $ \case
        C.ELetF b e -> countUse b >> e
        C.ECaseF _ v les -> countUse v >> mapM_ snd les
        C.EReturnF v -> countUse v
        C.EAnnotF e _ -> e

instance CountUse C.Bind where
    countUse (C.BVal (x, _) v)        = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BCall (x, _) f vs)    = resetUseCount (x ^. unique) >> countUse f >> mapM_ countUse vs
    countUse (C.BOpCall (x, _) _ vs)  = resetUseCount (x ^. unique) >> mapM_ countUse vs
    countUse (C.BProj (x, _) v _)     = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BUnpack _ (x, _) v)   = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BMalloc (x, _) _)     = resetUseCount (x ^. unique)
    countUse (C.BUpdate (x, _) y _ v) = resetUseCount (x ^. unique) >> countUse y >> countUse v

instance CountUse C.Code where
    countUse (C.Code args body) = mapM_ (resetUseCount . view unique . fst) args >> countUse body

instance CountUse C.Defn where
    countUse (_, c) = countUse c
