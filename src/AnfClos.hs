module AnfClos (a2cProg) where

import Anf                        qualified as A
import Closure                    qualified as C
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Functor.Foldable
import Data.List                  qualified as List
import Debug.Trace
import Id

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = StateT Escapes (ReaderT Locals (WriterT [C.Def] IO))

findLocals :: C.Var -> CCM ()
findLocals x = do
    lcls <- ask
    escs <- get
    if view extern x || fst x `elem` lcls || fst x `elem` map fst escs
        then return ()
        else modify (++ [x])

removeLocals :: Escapes -> Locals -> Escapes
removeLocals escs lcls = filter (\x -> fst x `notElem` lcls) escs

appendDef :: C.Def -> CCM ()
appendDef = lift . lift . tell . List.singleton

a2cLit :: A.Lit -> C.Lit
a2cLit (A.LInt i) = C.LInt i

a2cTy :: A.Ty -> C.Ty
a2cTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.TFun ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

a2cClosTy :: A.Ty -> C.Ty
a2cClosTy = cata $ \case
    A.TIntF        -> C.TInt
    A.TNameF x     -> C.TName x
    A.TFunF ts1 t2 -> C.mkClos ts1 t2
    A.TTupleF ts   -> C.TRow $ foldr (C.:>) C.REmpty ts

a2cVar :: A.Var -> C.Var
a2cVar (x, t) = (x, a2cTy t)

a2cClosVar :: A.Var -> C.Var
a2cClosVar (x, t) = (x, a2cClosTy t)

a2cVal :: A.Val -> CCM C.Val
a2cVal = cata $ \case
    A.VLitF l -> return $ C.VLit $ a2cLit l
    A.VVarF x | x ^. extern -> do
        let x' = a2cVar x
        return $ C.VVar x'
    A.VVarF x -> do
        let x' = a2cClosVar x
        findLocals x'
        return $ C.VVar x'
    A.VLabF l t -> return $ C.VLab l (a2cTy t)
    A.VLamF xs e -> do
        let xs' = map a2cClosVar xs
            lcls = map fst xs
        (e', escs) <- lift $ runStateT (local (const lcls) $ a2cExp e) []
        let escs' = removeLocals escs lcls
        modify $ List.nub . (escs ++)
        let r_env = foldr ((C.:>) . snd) C.REmpty escs'
            t_env = C.TRow r_env
            t_ucl = C.mkUClos (map snd xs') (C.typeof e') r_env
            t_cl = C.mkClos (map snd xs') (C.typeof e')
            x_cl = (mkIdUnsafe "x_cl", t_ucl)
            t_code = C.TFun (t_ucl : map snd xs') (C.typeof e')
        f_code <- (,t_code) <$> mkId "f_code"
        let v_code = C.Def {
                C.code = f_code,
                C.args = x_cl : xs',
                C.body =
                    let x_env = (mkIdUnsafe "x_env", t_env) in
                    if r_env == C.REmpty then e'
                    else
                        let d = C.BProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                        let ds = zipWith (\x i -> C.BProj x (C.VVar x_env) i) escs' [1..] in
                        foldr C.ELet e' (d : ds)
                }
        appendDef v_code
        return $ C.VPack t_env (C.VRoll (C.VTuple (C.VGlb f_code : map C.VVar escs')) t_ucl) t_cl
    A.VTupleF vs -> C.VTuple <$> sequence vs
    A.VValTyF mv t -> C.VValTy <$> mv <*> pure (a2cClosTy t)

a2cBind :: A.Bind -> CCM [C.Bind]
a2cBind (A.BVal x v)       = List.singleton <$> (C.BVal (a2cClosVar x) <$> a2cVal v)
a2cBind (A.BCall x v1@(A.VVar f) vs2) | f ^. extern = do
    v1' <- a2cVal v1
    vs2' <- mapM a2cVal vs2
    return [C.BCall (a2cVar x) v1' vs2']
a2cBind (A.BCall x v1 vs2)
    | C.TEx t_cl <- a2cClosTy (A.typeof v1) = do
    let x_cl = (mkIdUnsafe "x_cl", t_cl)
    d1 <- C.BUnpack x_cl <$> a2cVal v1
    let t_code = C.TFun (t_cl : map (a2cClosTy . A.typeof) vs2) (a2cClosTy (A.typeof x))
    let x_code = (mkIdUnsafe "x_code", t_code)
    let d2 = C.BProj x_code (C.VUnroll (C.VVar x_cl)) 1
    d3 <- C.BCall (a2cClosVar x) (C.VVar x_code) <$> ((C.VVar x_cl :) <$> mapM a2cVal vs2)
    return [d1, d2, d3]
    | otherwise = error "impossible"

-- | ex. @xs = [x1, x2, x3]@ -> @rotate 1 xs = [x2, x3, x1]@
rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

a2cExp :: A.Exp -> CCM C.Exp
a2cExp = cata $ \case
    A.ELetF d me -> flip (foldr C.ELet) <$> a2cBind d <*> local (fst (A.bindVar d):) me
    -- @ds@ is not empty
    A.ELetrecF ds me -> do
        let n = length ds
        predata <- mapM a2cRecBind ds
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
                x_cl = (mkIdUnsafe "x_cl", t_ucl)
                t_code = C.TFun (t_ucl : map snd xs) (C.typeof e)
            f_code <- (,t_code) <$> mkId (fst f ^. name  ++ "_code")
            traceShowM $ tail $ rotate (i-1) fs
            let v_code = C.Def {
                    C.code = f_code,
                    C.args = x_cl : xs,
                    C.body =
                        let di = C.BVal f $ C.VPack t_env (C.VVar x_cl) t_cl in
                        if r_env == C.REmpty then C.ELet di e
                        else
                            let x_env = (mkIdUnsafe "x_env", t_env) in
                            let d_env = C.BProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                            let ds_cl = zipWith (\fj j -> C.BProj fj (C.VVar x_env) j) (tail $ rotate (i-1) fs) [1..] in
                            let ds_esc = zipWith (\x j -> C.BProj x (C.VVar x_env) j) escs [n..] in
                            foldr C.ELet e (di : d_env : ds_cl ++ ds_esc)
                    }
            appendDef v_code
            let v = C.VPack t_env (C.VRoll (C.VTuple (C.VGlb f_code : map C.VVar escs)) t_ucl) t_cl
            return $ C.BVal f v
        flip (foldr C.ELet) ds' <$> me
    A.ECaseF v les -> C.ECase <$> a2cVal v <*> mapM (\(li, ei) -> (li,) <$> ei) les
    A.ERetF v -> C.ERet <$> a2cVal v
    A.EExpTyF me _ -> C.EExpTy <$> me <*> (C.typeof <$> me)

a2cRecBind :: A.Bind -> CCM (C.Var, [C.Var], C.Exp, [C.Var])
a2cRecBind (A.BVal f v) | A.VLam xs e <- stripAnnTop v = do
    let xs' = map a2cClosVar xs
    (e', escs) <- lift $ runStateT (local (const $ map fst (f : xs)) $ a2cExp e) []
    return (a2cClosVar f, xs', e', escs)
  where
    stripAnnTop :: A.Val -> A.Val
    stripAnnTop (A.VValTy v' _) = stripAnnTop v'
    stripAnnTop v'              = v'
a2cRecBind _ = error "impossible. lambda expected"

a2cDec :: A.Dec -> C.Dec
a2cDec (A.DEnum x ls) = C.DEnum x ls
a2cDec (A.DBind x t) | x ^. extern = C.DBind x (a2cTy t)
a2cDec (A.DBind x t) = C.DBind x (a2cClosTy t)

a2cProg :: A.Prog -> IO C.Prog
a2cProg (decs, e) = do
    (e', defs) <- runWriterT (runReaderT (evalStateT (a2cExp e) []) [])
    let decs' = foldr (C.extendBindEnv . C.code) (map a2cDec decs) defs
    return (decs', defs, e')
