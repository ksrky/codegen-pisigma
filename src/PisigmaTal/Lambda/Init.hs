module PisigmaTal.Lambda.Init (
    idBool,
    tyBool,
    primOps,
    initCtx,
    initEnv
) where

import PisigmaTal.Id
import PisigmaTal.Lambda
import PisigmaTal.Primitive

idBool :: Id
idBool = newIdUnsafe "Bool"

tyBool :: Ty
tyBool = TName idBool

idEq :: Id
idEq = newIdUnsafe "#eq"

primOps :: [(String, (PrimOp, Ty))]
primOps =
    [ ("+", (Add, TFun TInt (TFun TInt TInt)))
    , ("-", (Sub, TFun TInt (TFun TInt TInt)))
    , ("*", (Mul, TFun TInt (TFun TInt TInt)))
    , ("/", (Div, TFun TInt (TFun TInt TInt)))
    ]

initCtx :: Ctx
initCtx = Ctx
    { _varScope =
        [ ("==", (idEq, TFun TInt (TFun TInt tyBool)))
        ]
    , _labelScope =
        [ ("True", idBool)
        , ("False", idBool)
        ]
    }

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idEq (TFun TInt (TFun TInt tyBool))
    ]
