module Lambda.Init (
    tyBool,
    initCtx,
    initEnv
) where

import Id
import Lambda

idBool :: Id
idBool = newIdUnsafe "Bool"

tyBool :: Ty
tyBool = TName idBool

idPlus, idMinus, idTimes, idDiv :: Id
idPlus = newIdUnsafe "#add"
idMinus = newIdUnsafe "#sub"
idTimes = newIdUnsafe "#mul"
idDiv = newIdUnsafe "#div"

idEq :: Id
idEq = newIdUnsafe "#eq"

initCtx :: Ctx
initCtx = Ctx
    { _varScope =
        [ ("+", (idPlus, TFun TInt (TFun TInt TInt)))
        , ("-", (idMinus, TFun TInt (TFun TInt TInt)))
        , ("*", (idTimes, TFun TInt (TFun TInt TInt)))
        , ("/", (idDiv, TFun TInt (TFun TInt TInt)))
        , ("==", (idEq, TFun TInt (TFun TInt tyBool)))
        ]
    , _labelScope =
        [ ("True", TName idBool)
        , ("False", TName idBool)
        ]
    }

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idPlus (TFun TInt (TFun TInt TInt))
    , DBind idMinus (TFun TInt (TFun TInt TInt))
    , DBind idTimes (TFun TInt (TFun TInt TInt))
    , DBind idDiv (TFun TInt (TFun TInt TInt))
    , DBind idEq (TFun TInt (TFun TInt tyBool))
    ]
