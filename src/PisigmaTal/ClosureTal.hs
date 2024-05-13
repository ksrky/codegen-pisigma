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

freshName :: Monad m => Id -> TalBuilderT m T.Name
freshName x = newName (x ^. unique) (x ^. name)

freshNameFromString :: MonadIO m => String -> TalBuilderT m T.Name
freshNameFromString hint = freshName =<< newId hint

closureTalTy :: MonadFail m => C.Ty -> TalBuilderT m T.Ty
closureTalTy = cata $ \case
    C.TIntF -> return T.TInt
    C.TVarF x -> do
        tv <- getTyVar (x ^. unique)
        return $ T.TVar tv
    C.TFunF tys _ -> do
        sty <- mkAbstractStackTy 0 <$> sequence tys
        return $ T.TRegFile [()] (emptyRegFileTy & T.rfStackTy ?~ sty)
    C.TExistsF x ty -> T.TExists <$> withExtTyVarScope (x ^. unique) ty
    C.TRecursF x ty -> T.TRecurs <$> withExtTyVarScope (x ^. unique) ty
    C.TRowF row -> T.TRow <$> closureTalRowTy row
    C.TNameF x -> T.TAlias <$> lookupName (x ^. unique)

closureTalRowTy :: MonadFail m => C.RowTy -> TalBuilderT m T.RowTy
closureTalRowTy = cata $ \case
    C.REmptyF -> return T.REmpty
    C.RVarF x -> do
        tv <- getTyVar (x ^. unique)
        return $ T.RVar tv
    C.RSeqF ty rest -> T.RSeq <$> closureTalTy ty <*> rest

closureTalLit :: C.Lit -> T.WordVal
closureTalLit (C.LInt i) = T.VInt i

closureTalVal :: C.Val -> TalBuilder T.SmallVal
closureTalVal (C.VLit l) = return $ T.VWord $ closureTalLit l
closureTalVal (C.VVar (x, _)) = do
    reg <- findReg $ x ^. unique
    decUseCount (x ^. unique)
    whenUseCountZero (x ^. unique) $ setRegFree reg
    return $ T.VReg reg
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
    withExtRegTable (x ^. unique) reg $ withExtRegFileTy reg ty' cont

withExtRegs :: [C.Var] -> [T.Reg] -> TalBuilder a -> TalBuilder a
withExtRegs [] [] cont = cont
withExtRegs (var : vars) (reg : regs) cont = do
    withExtReg var reg $ withExtRegs vars regs cont
withExtRegs _ _ _ = error "impossible"

closureTalExp :: C.Exp -> TalBuilder T.Instrs
closureTalExp (C.ELet (C.BVal var val) exp) = do
    reg <- buildMove =<< closureTalVal val
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BCall var fun vals) exp) = do
    val' <- closureTalVal $ C.VVar fun
    vals' <- mapM closureTalVal vals
    arg_regs <- mapM buildMove vals'
    mapM_ setRegFree arg_regs
    tmp_regs <- getInUseRegs
    mapM_ buildPush tmp_regs
    mapM_ buildPush arg_regs
    extInstr $ T.ICall val'
    mapM_ buildPop tmp_regs
    withExtReg var RVReg $ closureTalExp exp
closureTalExp (C.ELet (C.BOpCall var prim _ vals) exp) | [val1, val2] <- vals = do
    reg1 <- buildMove =<< closureTalVal val1
    yes_free <- isFreeReg reg1
    reg <- if yes_free then setRegInUse reg1 >> return reg1 else freshReg
    val2' <- closureTalVal val2
    extInstr $ T.IAop (mapPrimop prim) reg reg1 val2'
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet C.BOpCall{} _) = error "impossible" -- tmp
closureTalExp (C.ELet (C.BProj var val idx) exp) = do
    reg <- buildMove =<< closureTalVal val
    extInstr $ T.ILoad reg reg (pred (fromEnum idx))
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BUnpack tv var val) exp) = do
    reg <- freshReg
    val' <- closureTalVal val
    extInstr $ T.IUnpack reg val'
    withExtTyVarScope (tv ^. unique) $ withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BMalloc var tys) exp) = do
    reg <- freshReg
    tys' <- mapM closureTalTy tys
    extInstr $ T.IMalloc reg tys'
    withExtReg var reg $ closureTalExp exp
closureTalExp (C.ELet (C.BUpdate var var' idx val) exp) = do
    reg <- buildMove =<< closureTalVal (C.VVar var')
    reg' <- buildMove =<< closureTalVal val
    extInstr $ T.IStore reg (pred (fromEnum idx)) reg'
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
    extInstr $ T.IMove RVReg val'
    buildInstrs $ T.IHalt ty
closureTalExp (C.EAnnot exp _) = closureTalExp exp

closureTalDefn :: C.Defn -> TalBuilder ()
closureTalDefn (var, C.Code args body) = do
    name' <- lookupName (fst var ^. unique)
    arg_tys <- mapM (closureTalTy . snd) args
    arg_regs <- mapM (const freshReg) args
    mapM_ buildPop arg_regs
    mapM_ setRegInUse arg_regs
    instrs <- withExtRegs args arg_regs $ closureTalExp body
    let rfty = emptyRegFileTy & T.rfStackTy ?~ mkAbstractStackTy 0 arg_tys
    extendHeap name' $ T.HCode [()] rfty instrs

closureTalDec :: C.Dec -> WriterT UserContext (TalBuilderT IO) (T.Name, T.Heap)
closureTalDec (C.DEnum con labs) = do
    scribe enums (M.singleton con labs)
    name' <- lift $ freshName con
    return (name', T.HTypeAlias T.TInt)
closureTalDec (C.DBind x ty) = lift $ do
    name' <- freshName x
    ty' <- closureTalTy ty
    return (name', T.HExtern ty')

closureTalProgram :: C.Program -> IO T.Program
closureTalProgram (decs, (defns, exp)) = do
    (heaps, user_ctx) <- runTalBuilderT initBuilderContext initBuilderState $ runWriterT $ mapM closureTalDec decs
    (`runReaderT` user_ctx) $
        runTalBuilderT initBuilderContext (initBuilderState & heapsState .~ M.fromList heaps) $ do
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
    countUse (C.BVal (x, _) v)         = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BCall (x, _) f vs)     = resetUseCount (x ^. unique) >> countUse f >> mapM_ countUse vs
    countUse (C.BOpCall (x, _) _ _ vs) = resetUseCount (x ^. unique) >> mapM_ countUse vs
    countUse (C.BProj (x, _) v _)      = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BUnpack _ (x, _) v)    = resetUseCount (x ^. unique) >> countUse v
    countUse (C.BMalloc (x, _) _)      = resetUseCount (x ^. unique)
    countUse (C.BUpdate (x, _) y _ v)  = resetUseCount (x ^. unique) >> countUse y >> countUse v

instance CountUse C.Code where
    countUse (C.Code args body) = mapM_ (resetUseCount . view unique . fst) args >> countUse body

instance CountUse C.Defn where
    countUse (_, c) = countUse c
