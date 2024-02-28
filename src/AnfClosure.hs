module AnfClosure (anfClosureProgram) where

import Anf                        qualified as A
import Closure                    qualified as C
import Id

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Functor.Foldable
import Data.List                  qualified as List

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = StateT Escapes (ReaderT Locals (WriterT [C.Defn] IO))

findLocals :: C.Var -> CCM ()
findLocals x = do
    lcls <- ask
    escs <- get
    if view extern x || fst x `elem` lcls || fst x `elem` map fst escs
        then return ()
        else modify (++ [x])

removeLocals :: Escapes -> Locals -> Escapes
removeLocals escs lcls = filter (\x -> fst x `notElem` lcls) escs

appendDefn :: C.Defn -> CCM ()
appendDefn = lift . lift . tell . List.singleton

anfClosureLit :: A.Lit -> C.Lit
anfClosureLit (A.LInt i) = C.LInt i

anfClosureTy :: A.Ty -> C.Ty
anfClosureTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.TFun ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

anfClosureClosTy :: A.Ty -> C.Ty
anfClosureClosTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.mkClos ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

anfClosureVar :: A.Var -> C.Var
anfClosureVar (x, t) = (x, anfClosureTy t)

anfClosureClosVar :: A.Var -> C.Var
anfClosureClosVar (x, t) = (x, anfClosureClosTy t)

anfClosureVal :: A.Val -> CCM C.Val
anfClosureVal = cata $ \case
    A.VLitF l -> return $ C.VLit $ anfClosureLit l
    A.VVarF x | x ^. extern -> do
        let x' = anfClosureVar x
        return $ C.VVar x'
    A.VVarF x -> do
        let x' = anfClosureClosVar x
        findLocals x'
        return $ C.VVar x'
    A.VLabelF l t -> return $ C.VLabel l (anfClosureTy t)
    A.VLamF xs e -> do
        let xs' = map anfClosureClosVar xs
            lcls = map fst xs
        (e', escs) <- lift $ runStateT (local (const lcls) $ anfClosureExp e) []
        let escs' = removeLocals escs lcls
        modify $ List.nub . (escs ++)
        let r_env = foldr ((C.:>) . snd) C.REmpty escs'
            t_env = C.TRow r_env
            t_ucl = C.mkUClos (map snd xs') (C.typeof e') r_env
            t_cl = C.mkClos (map snd xs') (C.typeof e')
            x_cl = (newIdUnsafe "x_cl", t_ucl)
            t_code = C.TFun (t_ucl : map snd xs') (C.typeof e')
        f_code <- (,t_code) <$> newId "f_code"
        let v_code = C.Defn {
                C.code = f_code,
                C.args = x_cl : xs',
                C.body =
                    let x_env = (newIdUnsafe "x_env", t_env) in
                    if r_env == C.REmpty then e'
                    else
                        let d = C.BProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                        let ds = zipWith (\x i -> C.BProj x (C.VVar x_env) i) escs' [1..] in
                        foldr C.ELet e' (d : ds)
                }
        appendDefn v_code
        return $ C.VPack t_env (C.VRoll (C.VTuple (C.VFun f_code : map C.VVar escs')) t_ucl) t_cl
    A.VTupleF vs -> C.VTuple <$> sequence vs
    A.VAnnotF mv t -> C.VAnnot <$> mv <*> pure (anfClosureClosTy t)

anfClosureBind :: A.Bind -> CCM [C.Bind]
anfClosureBind (A.BVal x v)       = List.singleton <$> (C.BVal (anfClosureClosVar x) <$> anfClosureVal v)
anfClosureBind (A.BCall x v1@(A.VVar f) vs2) | f ^. extern = do
    v1' <- anfClosureVal v1
    vs2' <- mapM anfClosureVal vs2
    return [C.BCall (anfClosureVar x) v1' vs2']
anfClosureBind (A.BCall x v1 vs2)
    | C.TExists t_cl <- anfClosureClosTy (A.typeof v1) = do
    let x_cl = (newIdUnsafe "x_cl", t_cl)
    d1 <- C.BUnpack x_cl <$> anfClosureVal v1
    let t_code = C.TFun (t_cl : map (anfClosureClosTy . A.typeof) vs2) (anfClosureClosTy (A.typeof x))
    let x_code = (newIdUnsafe "x_code", t_code)
    let d2 = C.BProj x_code (C.VUnroll (C.VVar x_cl)) 1
    d3 <- C.BCall (anfClosureClosVar x) (C.VVar x_code) <$> ((C.VVar x_cl :) <$> mapM anfClosureVal vs2)
    return [d1, d2, d3]
    | otherwise = error "impossible"

-- | ex. @xs = [x1, x2, x3]@ -> @rotate 1 xs = [x2, x3, x1]@
rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

anfClosureExp :: A.Exp -> CCM C.Exp
anfClosureExp = cata $ \case
    A.ELetF d me -> flip (foldr C.ELet) <$> anfClosureBind d <*> local (fst (A.bindVar d):) me
    -- @ds@ is not empty
    A.ELetrecF ds me -> do
        let n = length ds
        predata <- mapM anfClosureRecBind ds
        modify $ List.nub . (concatMap (\(_, ls, _, es) -> es List.\\ ls) predata ++)
        let t_excls = map (\(_, xs, e, _) -> C.mkClos (map snd xs) (C.typeof e)) predata
            fs = map (view _1) predata
        ds' <- forM [1..n] $ \i -> do
            let (f, xs, e, escs) = predata !! (i - 1)
            let r_escs = foldr ((C.:>) . snd) C.REmpty escs
                r_env = foldr (C.:>) r_escs (tail $ rotate (i-1) t_excls)
                t_env = C.TRow r_env
                t_ucl = C.mkUClos (map snd xs) (C.typeof e) r_env
                t_cl = C.mkClos (map snd xs) (C.typeof e)
                x_cl = (newIdUnsafe "x_cl", t_ucl)
                t_code = C.TFun (t_ucl : map snd xs) (C.typeof e)
            f_code <- (,t_code) <$> newId (fst f ^. name  ++ "_code")
            let v_code = C.Defn {
                    C.code = f_code,
                    C.args = x_cl : xs,
                    C.body =
                        let di = C.BVal f $ C.VPack t_env (C.VVar x_cl) t_cl in
                        if r_env == C.REmpty then C.ELet di e
                        else
                            let x_env = (newIdUnsafe "x_env", t_env) in
                            let d_env = C.BProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                            let ds_cl = zipWith (\fj j -> C.BProj fj (C.VVar x_env) j) (tail $ rotate (i-1) fs) [1..] in
                            let ds_esc = zipWith (\x j -> C.BProj x (C.VVar x_env) j) escs [n..] in
                            foldr C.ELet e (di : d_env : ds_cl ++ ds_esc)
                    }
            appendDefn v_code
            let v = C.VPack t_env (C.VRoll (C.VTuple (C.VFun f_code : map C.VVar escs)) t_ucl) t_cl
            return $ C.BVal f v
        flip (foldr C.ELet) ds' <$> me
    A.ECaseF v les -> C.ECase <$> anfClosureVal v <*> mapM (\(li, ei) -> (li,) <$> ei) les
    A.EReturnF v -> C.EReturn <$> anfClosureVal v
    A.EAnnotF me _ -> C.EAnnot <$> me <*> (C.typeof <$> me)

anfClosureRecBind :: A.Bind -> CCM (C.Var, [C.Var], C.Exp, [C.Var])
anfClosureRecBind (A.BVal f v) | A.VLam xs e <- stripAnnotTop v = do
    let xs' = map anfClosureClosVar xs
    (e', escs) <- lift $ runStateT (local (const $ map fst (f : xs)) $ anfClosureExp e) []
    return (anfClosureClosVar f, xs', e', escs)
  where
    stripAnnotTop :: A.Val -> A.Val
    stripAnnotTop (A.VAnnot v' _) = stripAnnotTop v'
    stripAnnotTop v'              = v'
anfClosureRecBind _ = error "impossible. lambda expected"

anfClosureDec :: A.Dec -> C.Dec
anfClosureDec (A.DEnum x ls) = C.DEnum x ls
anfClosureDec (A.DBind x t) | x ^. extern = C.DBind x (anfClosureTy t)
anfClosureDec (A.DBind x t) = C.DBind x (anfClosureClosTy t)

anfClosureProgram :: A.Program -> IO C.Program
anfClosureProgram (decs, e) = do
    (e', defs) <- runWriterT (runReaderT (evalStateT (anfClosureExp e) []) [])
    let decs' = foldr (C.extendBindEnv . C.code) (map anfClosureDec decs) defs
    return (decs', defs, e')
