module AnfToClos (a2cProg) where

import Anf                      qualified as A
import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.RWS
import Data.Functor.Foldable
import Data.List                qualified as List
import Data.Tuple               qualified as Tuple
import Id

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = RWST Locals [C.Def] Escapes IO

resetEscapes :: ([C.Var] -> [C.Var]) -> CCM [C.Var]
resetEscapes new = do
    escs <- get
    put (new escs)
    return escs

findLocals :: C.Var -> CCM ()
findLocals x = do
    lcls <- ask
    escs <- get
    if view extern x || fst x `elem` lcls || fst x `elem` map fst escs
        then return ()
        else modify (++ [x])

appendDef :: C.Def -> CCM ()
appendDef = tell . List.singleton

a2cLit :: A.Lit -> C.Lit
a2cLit (A.LInt i) = C.LInt i

a2cTy :: A.Ty -> C.Ty
a2cTy = cata $ \case
    A.TIntF       -> C.TInt
    A.TFunF ts1 t2 -> mkTEx ts1 t2

mkTEx :: [C.Ty] -> C.Ty -> C.Ty
mkTEx ts t =
    let tv_env = localId "t_env" in
    let tv_cl = localId "t_cl" in
    C.TEx tv_env $ C.TRec tv_cl $ C.TRow $ C.RSeq (C.TFun (C.TVar tv_cl : ts) t) (C.RVar tv_env)

a2cVar :: A.Var -> C.Var
a2cVar (x, t) = (x, a2cTy t)

a2cVal :: A.Val -> CCM C.Val
a2cVal = cata $ \case
    A.VLitF l -> return $ C.VLit $ a2cLit l
    A.VVarF x -> do
        let x' = a2cVar x
        findLocals x'
        return $ C.VVar x'
    A.VLamF xs e -> do
        let xs' = map a2cVar xs
        escs <- resetEscapes $ const []
        e' <- local (const $ map fst xs) $ a2cExp e
        escs' <- resetEscapes (escs ++)
        let r_env = foldr (C.RSeq . snd) C.REmpty escs'
            t_env = C.TRow r_env
            t_cl =
                let tv_cl = localId "tv_cl" in
                C.TRec tv_cl $ C.TRow $
                    C.RSeq (C.TFun (C.TVar tv_cl : map snd xs') (C.typeof e')) r_env
            x_cl = (localId "x_cl", t_cl)
            t_code = C.TFun (t_cl : map snd xs') (C.typeof e')
        x_code <- (,t_code) <$> fromString "x_code"
        let v_code = C.Def {
                C.name = x_code,
                C.args = x_cl : xs',
                C.body =
                    let x_env = (localId "x_env", t_env) in
                    if r_env == C.REmpty then e'
                    else
                        let d = C.DProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                        let ds = zipWith (\x i -> C.DProj x (C.VVar x_env) i) escs' [1..] in
                        foldr C.ELet e' (d : ds)
                }
        appendDef v_code
        return $ C.VPack t_env (C.VRoll (C.VTuple (C.VGlb x_code : map C.VVar escs')) t_cl) t_cl
    A.VValTyF mv t -> C.VValTy <$> mv <*> pure (a2cTy t)

a2cDec :: A.Dec -> CCM [C.Dec]
a2cDec (A.DVal x v)       = List.singleton <$> (C.DVal (a2cVar x) <$> a2cVal v)
a2cDec (A.DCall x v1@(A.VVar f) vs2) | view extern f = do
    v1' <- a2cVal v1
    vs2' <- mapM a2cVal vs2
    return [C.DCall (a2cVar x) v1' vs2']
a2cDec (A.DCall x v1 vs2) = do
    let (tv_env, t_cl) = case a2cTy (A.typeof v1) of
            t@(C.TEx tv _) -> (tv, t)
            _              -> error "impossible"
    let x_cl = (localId "x_cl", t_cl)
    let t_code = C.TFun (t_cl : map (a2cTy . A.typeof) vs2) (a2cTy (A.typeof x))
    let x_code = (localId "x_code", t_code)
    d1 <- C.DUnpack tv_env x_cl <$> a2cVal v1
    let d2 = C.DProj x_code (C.VVar x_cl) 1
    d3 <- C.DCall (a2cVar x) (C.VVar x_code) <$> ((C.VVar x_cl :) <$> mapM a2cVal vs2)
    return [d1, d2, d3]

getDecId :: A.Dec -> Id
getDecId (A.DVal x _)    = fst x
getDecId (A.DCall x _ _) = fst x

-- | ex. @xs = [x1, x2, x3]@ -> @rotate 1 xs = [x2, x3, x1]@
rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

a2cExp :: A.Exp -> CCM C.Exp
a2cExp = cata $ \case
    A.ELetF d me -> flip (foldr C.ELet) <$> a2cDec d <*> local (getDecId d:) me
    -- @ds@ is not empty
    A.ELetrecF ds me -> do
        let n = length ds
        predata <- mapM a2cRecDec ds
        put $ List.nub $ concatMap (view _4) predata
        let t_excls = map (\(_, xs, e, _) -> mkTEx (map snd xs) (C.typeof e)) predata
            fs = map (view _1) predata
        ds' <- forM [1..n] $ \i -> do
            let (f, xs, e, escs) = predata !! (i - 1)
            let r_escs = foldr (C.RSeq . snd) C.REmpty escs
                r_env = foldr C.RSeq r_escs (tail $ rotate (i-1) t_excls)
                t_env = C.TRow r_env
                tv_cl = localId $ "tv_cl" ++ show i
                t_cl = C.TRec tv_cl $ C.TRow $ C.RSeq (C.TFun (map snd xs) (C.typeof e)) r_env
                t_excl = mkTEx (map snd xs) (C.typeof e)
                x_cl = (localId "x_cl", t_cl)
                t_code = C.TFun (t_cl : map snd xs) (C.typeof e)
            x_code <- (,t_code) <$> fromString (fst f ^. name  ++ "_code")
            let v_code = C.Def {
                    C.name = x_code,
                    C.args = x_cl : xs,
                    C.body =
                        let di = C.DVal f $ C.VPack t_env (C.VVar x_cl) t_excl in
                        if r_env == C.REmpty then C.ELet di e
                        else
                            let x_env = (localId "x_env", t_env) in
                            let d_env = C.DProj x_env (C.VUnroll (C.VVar x_cl)) 2 in
                            let ds_cl = zipWith (\fj j -> C.DProj fj (C.VVar x_env) j) (tail $ rotate (i-1) fs) [1..] in
                            let ds_esc = zipWith (\x j -> C.DProj x (C.VVar x_env) j) escs [n..] in
                            foldr C.ELet e (di : d_env : ds_cl ++ ds_esc)
                    }
            appendDef v_code
            let v = C.VPack t_env (C.VRoll (C.VTuple (C.VGlb x_code : map C.VVar escs)) t_cl) t_excl
            return $ C.DVal f v
        flip (foldr C.ELet) ds' <$> me
    A.ERetF v -> C.ERet <$> a2cVal v
    A.EExpTyF me t -> C.EExpTy <$> me <*> pure (a2cTy t)

a2cRecDec :: A.Dec -> CCM (C.Var, [C.Var], C.Exp, [C.Var])
a2cRecDec (A.DVal f (A.VLam xs e)) = do
    let xs' = map a2cVar xs
    escs <- resetEscapes $ const []
    e' <- local (const $ map fst xs) $ a2cExp e
    escs' <- resetEscapes (const escs)
    return (a2cVar f, xs', e', escs')
a2cRecDec _ = error "not implemented"

a2cProg :: A.Prog -> IO C.Prog
a2cProg e = Tuple.swap <$> evalRWST (a2cExp e) [] []
