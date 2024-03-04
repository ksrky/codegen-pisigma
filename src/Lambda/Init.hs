module Lambda.Init (
    tyBool,
    initCtx,
    initEnv
) where

import Control.Lens.Operators
import Id
import Lambda

idBool :: Id
idBool = newIdUnsafe "Bool"

tyBool :: Ty
tyBool = TName idBool

idTrue :: Id
idTrue = newIdUnsafe "True"

idFalse :: Id
idFalse = newIdUnsafe "False"

idPlus :: Id
idPlus = newIdUnsafe "#add" & extern .~ True

idMinus :: Id
idMinus = newIdUnsafe "#sub" & extern .~ True

idTimes :: Id
idTimes = newIdUnsafe "#mul" & extern .~ True

idDiv :: Id
idDiv = newIdUnsafe "#div" & extern .~ True

idEq :: Id
idEq = newIdUnsafe "#eq" & extern .~ True

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
