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

idTrue, idFalse :: Id
idTrue = newIdUnsafe "True"
idFalse = newIdUnsafe "False"

idPlus, idMinus, idTimes, idDiv :: Id
idPlus = newIdUnsafe "#add"
idMinus = newIdUnsafe "#sub"
idTimes = newIdUnsafe "#mul"
idDiv = newIdUnsafe "#div"

idEq :: Id
idEq = newIdUnsafe "#eq"

initCtx :: [(String, (Id, Ty))]
initCtx =
    [ ("True", (idTrue, TName idBool))
    , ("False", (idFalse, TName idBool))
    , ("+", (idPlus, TFun TInt (TFun TInt TInt)))
    , ("-", (idMinus, TFun TInt (TFun TInt TInt)))
    , ("*", (idTimes, TFun TInt (TFun TInt TInt)))
    , ("/", (idDiv, TFun TInt (TFun TInt TInt)))
    , ("==", (idEq, TFun TInt (TFun TInt tyBool)))
    ]

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idPlus (TFun TInt (TFun TInt TInt))
    , DBind idMinus (TFun TInt (TFun TInt TInt))
    , DBind idTimes (TFun TInt (TFun TInt TInt))
    , DBind idDiv (TFun TInt (TFun TInt TInt))
    , DBind idEq (TFun TInt (TFun TInt tyBool))
    ]
