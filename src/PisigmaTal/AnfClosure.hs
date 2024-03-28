module PisigmaTal.AnfClosure (anfClosureProgram) where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Functor.Foldable
import Data.List                  qualified as List
import PisigmaTal.Anf             qualified as A
import PisigmaTal.Closure         qualified as C
import PisigmaTal.Id
import PisigmaTal.Idx
import Prelude                    hiding (exp)

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = StateT Escapes (ReaderT Locals (WriterT [C.Defn] IO))

findLocals :: C.Var -> CCM ()
findLocals x = do
    lcls <- ask
    escs <- get
    if fst x `elem` lcls || fst x `elem` map fst escs
        then return ()
        else modify (|> x)

removeLocals :: Escapes -> Locals -> Escapes
removeLocals escs lcls = filter (\x -> fst x `notElem` lcls) escs

appendDefn :: C.Defn -> CCM ()
appendDefn = lift . lift . tell . List.singleton

anfClosureLit :: A.Lit -> C.Lit
anfClosureLit (A.LInt i) = C.LInt i

anfClosureKnownTy :: A.Ty -> C.Ty
anfClosureKnownTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.TFun ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

anfClosureTy :: A.Ty -> C.Ty
anfClosureTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.ClosTy ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

anfClosureKnownVar :: A.Var -> C.Var
anfClosureKnownVar (x, t) = (x, anfClosureKnownTy t)

anfClosureVar :: A.Var -> C.Var
anfClosureVar (x, t) = (x, anfClosureTy t)

anfClosureVal :: A.Val -> CCM C.Val
anfClosureVal = cata $ \case
    A.VLitF l -> return $ C.VLit $ anfClosureLit l
    A.VVarF x -> do
        let x' = anfClosureVar x
        findLocals x'
        return $ C.VVar x'
    A.VLabelF l t -> return $ C.VLabel l (anfClosureTy t)
    A.VLamF xs exp -> do
        let xs' = map anfClosureVar xs
            lcls = map fst xs
        (exp', escs) <- lift $ runStateT (local (const lcls) $ anfClosureExp exp) []
        let escs' = removeLocals escs lcls
        modify $ List.nub . (escs ++)
        let r_env = foldr ((C.:>) . snd) C.REmpty escs'
            t_env = C.TRow r_env
            t_ucl = C.UClosTy (map snd xs') (C.typeof exp') r_env
            t_cl = C.ClosTy (map snd xs') (C.typeof exp')
            t_code = C.TFun (t_ucl : map snd xs') (C.typeof exp')
        x_cl <- (,t_ucl) <$> newId "x_cl"
        let binds = zipWith (\x i -> C.BProj x (C.VUnroll (C.VVar x_cl)) (intToIdx i)) escs' [2 ..]
            v_code = C.Code {
                C.args = x_cl : xs',
                C.body = foldr C.ELet exp' binds
            }
        f_code <- (,t_code) <$> newId "f_code"
        appendDefn (f_code, v_code)
        return $ C.Clos t_env (C.VFun f_code : map C.VVar escs') t_cl
    A.VTupleF vs -> C.VTuple <$> sequence vs
    A.VAnnotF mval ty -> C.VAnnot <$> mval <*> pure (anfClosureTy ty)

anfClosureBind :: A.Bind -> CCM [C.Bind]
anfClosureBind (A.BVal x v) = List.singleton <$> (C.BVal (anfClosureVar x) <$> anfClosureVal v)
anfClosureBind (A.BPartialApp x v1 vs2)
    | C.TExists tv t_cl <- anfClosureTy (A.typeof v1) = do
    x_cl <- (,t_cl) <$> newId "x_cl"
    d1 <- C.BUnpack tv x_cl <$> anfClosureVal v1
    let t_code = C.TFun (t_cl : map (anfClosureTy . A.typeof) vs2) (anfClosureTy (A.typeof x))
    x_code <- (,t_code) <$> newId "x_code"
    let d2 = C.BProj x_code (C.VUnroll (C.VVar x_cl)) Idx1
    d3 <- C.BCall (anfClosureVar x) (C.LocalFun x_code) <$> ((C.VVar x_cl :) <$> mapM anfClosureVal vs2)
    return [d1, d2, d3]
    | otherwise = error "impossible"
anfClosureBind (A.BFullApp x f vs2) = do
    vs2' <- mapM anfClosureVal vs2
    return [C.BCall (anfClosureVar x) (C.KnownFun (anfClosureKnownVar f)) vs2']

anfClosureExp :: A.Exp -> CCM C.Exp
anfClosureExp = cata $ \case
    A.ELetF d me -> flip (foldr C.ELet) <$> anfClosureBind d <*> local (fst (A.bindVar d):) me
    A.ELetrecF binds mexp -> do
        binds' <- forM binds $ \(A.RecBind f vars exp) -> do
            let f' = anfClosureVar f
            let vars' = map anfClosureVar vars
            (exp', escs) <- lift $ runStateT (local (const $ map fst (f : vars)) $ anfClosureExp exp) []
            modify (escs ++)
            let r_env = foldr ((C.:>) . snd) C.REmpty escs
                t_env = C.TRow r_env
                var_tys = map snd vars'
                ret_ty = C.typeof exp'
                t_ucl = C.UClosTy var_tys ret_ty r_env
                t_cl = C.ClosTy var_tys ret_ty
                t_code = C.TFun (t_ucl : var_tys) ret_ty
            x_cl <- (,t_ucl) <$> newId "x_cl"
            let bind0 = C.BVal f' $ C.VPack t_env (C.VVar x_cl) t_cl
                binds' = zipWith (\x j -> C.BProj x (C.VUnroll (C.VVar x_cl)) (intToIdx j)) escs [2 ..]
                v_code = C.Code {
                    C.args = x_cl : vars',
                    C.body = foldr C.ELet exp' (bind0 : binds')
                }
            f_code <- (,t_code) <$> newId (fst f ^. name  ++ "_code")
            appendDefn (f_code, v_code)
            -- let uclos = C.UClos (C.VFun f_code : map C.VVar escs) t_ucl
            -- return (f', (t_env, uclos, t_cl))
            return $ C.BVal f' (C.Clos t_env (C.VFun f_code : map C.VVar escs) t_cl)
        modify List.nub
        -- C.ELet (C.BFixpack closures) <$> mexp
        C.ELetrec binds' <$> mexp
    A.ECaseF v les -> C.ECase <$> anfClosureVal v <*> mapM (\(li, ei) -> (li,) <$> ei) les
    A.EReturnF v -> C.EReturn <$> anfClosureVal v
    A.EAnnotF mexp ty -> C.EAnnot <$> mexp <*> pure (anfClosureTy ty)

anfClosureDec :: A.Dec -> C.Dec
anfClosureDec (A.DEnum x ls) = C.DEnum x ls
anfClosureDec (A.DBind x t)  = C.DBind x (anfClosureKnownTy t)

anfClosureProgram :: A.Program -> IO C.Program
anfClosureProgram (decs, exp) = do
    (exp', defns) <- runWriterT (runReaderT (evalStateT (anfClosureExp exp) []) [])
    let decs' = map anfClosureDec decs
    return (decs', (defns, exp'))
