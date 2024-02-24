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
externPlus = DBind idPlus (TFun (TTuple [TInt, TInt]) TInt)

idTimes :: Id
idTimes = mkIdUnsafe "*" & extern .~ True

externTimes :: Dec
externTimes = DBind idTimes (TFun (TTuple [TInt, TInt]) TInt)

idEq :: Id
idEq = mkIdUnsafe "==" & extern .~ True

externEq :: Dec
externEq = DBind idEq (TFun (TTuple [TInt, TInt]) tyBool)

initCtx :: [(String, (Id, Ty))]
initCtx =
    [ ("True", (idTrue, TName idBool)) -- tmp: idTrue is redundant
    , ("False", (idFalse, TName idBool))
    , ("+", (idPlus, TFun (TTuple [TInt, TInt]) TInt))
    , ("*", (idTimes, TFun (TTuple [TInt, TInt]) TInt))
    , ("==", (idEq, TFun (TTuple [TInt, TInt]) tyBool))
    ]

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idPlus (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idTimes (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idEq (TFun (TTuple [TInt, TInt]) tyBool)
    ]
