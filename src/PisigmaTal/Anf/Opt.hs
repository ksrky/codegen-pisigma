module PisigmaTal.Anf.Opt () where

import Control.Lens.Combinators
import Data.Map.Strict          qualified as M
import PisigmaTal.Anf
import PisigmaTal.Anf.Monad
import PisigmaTal.Id

-- TODO: optimization
-- 1. curried functions may create unnecessary closures
--    detect some inner lambda expressions. eg. \x -> \y -> (+) x y to \x y -> (+) x y
-- 2. inlining
--    - inline small functions
--      (\x y -> e) v w to e[x := v, y := w] if e is not big (e has no multi-arity constructor)
--    - inline functions that are only called once, then check the conditions below and inline.
--    - inline lambda expressions (beta reduction)
--      (\x -> e1) e2 to e1[x := e2] if x occurs only once in e1 and e2 is not big
-- 3. constant folding
--    - fold constant expressions
--    - eg. (+) 1 2 to 3
-- 4. dead code elimination
--    - remove unused bindings
--    - eg. let x = 1 in 2 to 2
-- 5. defunctionalization (option)
--    - replace higher-order functions with data types and pattern matching
--    id (id := \x -> x) to \x -> id x and make sure not to be closure-converted
--    f x (f := \x -> \y -> e) to \y -> f x y

newtype OptContext = OptContext
    { _optEnv :: M.Map Id Val
    }

isSmall :: Val -> Bool
isSmall VLam{}       = False
isSmall VTuple{}     = False
isSmall (VAnnot v _) = isSmall v
isSmall _            = True

class Optimization a where
    optimize :: a -> AnfM a

instance Optimization Val where
    optimize (VLit l)     = return $ VLit l
    optimize (VVar x)     = return $ VVar x
    optimize (VLabel c l) = return $ VLabel c l
    optimize (VLam xs e)  = VLam xs <$> optimize e
    optimize (VTuple vs)  = VTuple <$> mapM optimize vs
    optimize (VAnnot v t) = VAnnot <$> optimize v <*> pure t

instance Optimization Bind where
    optimize (BVal x v)        = BVal x <$> optimize v
    optimize (BApp x f vs)
        | VLam ys (EReturn v) <- stripAnnotTop f
        , length ys == length vs && and [isSmall v | v <- vs]
        = optimize v
    optimize (BApp x f vs)     = BApp x <$> optimize f <*> mapM optimize vs
    optimize (BFullApp x f vs) = BFullApp x f <$> mapM optimize vs

instance Optimization Exp where
    optimize :: Exp -> AnfM Exp
    optimize (ELet b e) = do
        cnt <- readIdCount (fst (bindVar b))
        case cnt of
            0                 -> optimize e
            1 | BVal x v <- b -> optimize e -- withEnv x v $ optimize e
            _                 -> ELet <$> optimize b <*> optimize e
    optimize (ELetrec bs e) = optimize e
    optimize (ECase c v alts) = ECase c <$> optimize v <*> mapM (\(l, e) -> (l,) <$> optimize e) alts
    optimize (EReturn v) = EReturn <$> optimize v
    optimize (EAnnot e t) = EAnnot <$> optimize e <*> pure t

