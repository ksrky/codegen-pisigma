module AnfToClos (a2cProg) where

import Anf                      qualified as A
import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Monad.RWS
import Data.Functor.Foldable
import Data.List                qualified as List
import Data.Tuple               qualified as Tuple
import Id

type Locals = [Id]

type Escapes = [C.Var]

-- | Closure Conversion Monad
type CCM = RWS Locals [C.Def] Escapes

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
    let tv_env = fromString "t_env" in
    let tv_cl = fromString "t_cl" in
    C.TEx tv_env $ C.TRec tv_cl $
        C.TRow $ C.RSeq (C.TFun (C.TVar tv_cl : ts) t) (C.RVar tv_env)

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
                let tv_cl = fromString "tv_cl" in
                C.TRec tv_cl $ C.TRow $
                    C.RSeq (C.TFun (C.TVar tv_cl : map snd xs') (C.typeof e')) r_env
            x_cl = (fromString "x_cl", t_cl)
            t_code = C.TFun (t_cl : map snd xs') (C.typeof e')
            x_code = (fromString "x_code", t_code)
            v_code = C.Def {
                C.name = x_code,
                C.args = x_cl : xs',
                C.body =
                    let x_env = (fromString "x_env", t_env) in
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
    let x_cl = (fromString "x_cl", t_cl)
    let t_code = C.TFun (t_cl : map (a2cTy . A.typeof) vs2) (a2cTy (A.typeof x))
    let x_code = (fromString "x_code", t_code)
    d1 <- C.DUnpack tv_env x_cl <$> a2cVal v1
    let d2 = C.DProj x_code (C.VVar x_cl) 1
    d3 <- C.DCall (a2cVar x) (C.VVar x_code) <$> ((C.VVar x_cl :) <$> mapM a2cVal vs2)
    return [d1, d2, d3]

getDecId :: A.Dec -> Id
getDecId (A.DVal x _)    = fst x
getDecId (A.DCall x _ _) = fst x

a2cExp :: A.Exp -> CCM C.Exp
a2cExp = cata $ \case
    A.ELetF d me -> flip (foldr C.ELet) <$> a2cDec d <*> local (getDecId d:) me
    A.ERetF v -> C.ERet <$> a2cVal v
    A.EExpTyF me t -> C.EExpTy <$> me <*> pure (a2cTy t)

a2cProg :: A.Prog -> C.Prog
a2cProg e = Tuple.swap $ evalRWS (a2cExp e) [] []
