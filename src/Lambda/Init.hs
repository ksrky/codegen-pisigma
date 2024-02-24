module Lambda.Init (
    tyBool,
    enumBool,
    externPlus,
    externTimes,
    externEq,
    initCtx,
    initEnv) where

import Control.Lens.Operators
import Id
import Lambda

idBool :: Id
idBool = mkIdUnsafe "Bool"

tyBool :: Ty
tyBool = TName idBool

idTrue :: Id
idTrue = mkIdUnsafe "True"

idFalse :: Id
idFalse = mkIdUnsafe "False"

enumBool :: Dec
enumBool = DEnum idBool ["True", "False"]

idPlus :: Id
idPlus = mkIdUnsafe "+" & extern .~ True

externPlus :: Dec
externPlus = DBind idPlus (TFun TInt (TFun TInt TInt))

idTimes :: Id
idTimes = mkIdUnsafe "*" & extern .~ True

externTimes :: Dec
externTimes = DBind idTimes (TFun TInt (TFun TInt TInt))

idEq :: Id
idEq = mkIdUnsafe "==" & extern .~ True

externEq :: Dec
externEq = DBind idEq (TFun TInt (TFun TInt tyBool))

initCtx :: [(String, (Id, Ty))]
initCtx =
    [ ("True", (idTrue, TName idBool)) -- tmp: idTrue is redundant
    , ("False", (idFalse, TName idBool))
    , ("+", (idPlus, TFun TInt (TFun TInt TInt)))
    , ("*", (idTimes, TFun TInt (TFun TInt TInt)))
    , ("==", (idEq, TFun TInt (TFun TInt tyBool)))
    ]

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idPlus (TFun TInt (TFun TInt TInt))
    , DBind idTimes (TFun TInt (TFun TInt TInt))
    , DBind idEq (TFun TInt (TFun TInt tyBool))
    ]
