{-# LANGUAGE TemplateHaskell #-}

module ClosureNameless where

import Closure                  qualified as C
import Control.Lens.Combinators
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.List                qualified as List
import Id
import Nameless                 qualified as N

data Ctx = Ctx {
    _varScope    :: [Id],
    _tyvarScope  :: [C.TyVar],
    _tyNameSCope :: [(String, Int)]
}

makeLenses ''Ctx

type CtxM = ReaderT Ctx IO

closureNamelessTy :: C.Ty -> CtxM N.Ty
closureNamelessTy = cata $ \case
    C.TIntF        -> return N.TInt
    C.TVarF x      -> do
        tvsc <- view tyvarScope
        case List.elemIndex x tvsc of
            Just i  -> return $ N.TVar i
            Nothing -> fail "unbound type variable"
    C.TNameF x     -> return $ N.TAlias undefined
    C.TFunF ts1 t2 -> N.TFun <$> sequence ts1 <*> t2
    C.TExistsF x t -> N.TExists <$> locally tyvarScope (x :) t
    C.TRecursF x t -> N.TRecurs <$> locally tyvarScope (x :) t
    C.TRowF r      -> undefined

closureNamelessLit :: C.Lit -> N.Const
closureNamelessLit (C.LInt i) = N.CInt i

closureNamelessVal :: C.Val -> CtxM N.Val
closureNamelessVal (C.VLit l)      = return $ N.VConst (closureNamelessLit l)
closureNamelessVal (C.VVar (x, t)) = do
    vsc <- view varScope
    case List.elemIndex x vsc of
        Just i  -> N.VVar i <$> closureNamelessTy t
        Nothing -> fail "unbound variable"
closureNamelessVal _               = undefined


