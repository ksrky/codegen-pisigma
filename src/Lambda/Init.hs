module Lambda.Init (
    tyBool,
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

idPlus :: Id
idPlus = mkIdUnsafe "#add" & extern .~ True

idMinus :: Id
idMinus = mkIdUnsafe "#sub" & extern .~ True

idTimes :: Id
idTimes = mkIdUnsafe "#mul" & extern .~ True

idDiv :: Id
idDiv = mkIdUnsafe "#div" & extern .~ True

idEq :: Id
idEq = mkIdUnsafe "#eq" & extern .~ True

externEq :: Dec
externEq = DBind idEq (TFun (TTuple [TInt, TInt]) tyBool)

initCtx :: [(String, (Id, Ty))]
initCtx =
    [ ("True", (idTrue, TName idBool)) -- tmp: idTrue is redundant
    , ("False", (idFalse, TName idBool))
    , ("+", (idPlus, TFun (TTuple [TInt, TInt]) TInt))
    , ("-", (idMinus, TFun (TTuple [TInt, TInt]) TInt))
    , ("*", (idTimes, TFun (TTuple [TInt, TInt]) TInt))
    , ("/", (idDiv, TFun (TTuple [TInt, TInt]) TInt))
    , ("==", (idEq, TFun (TTuple [TInt, TInt]) tyBool))
    ]

initEnv :: Env
initEnv =
    [ DEnum idBool ["True", "False"]
    , DBind idPlus (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idMinus (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idTimes (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idDiv (TFun (TTuple [TInt, TInt]) TInt)
    , DBind idEq (TFun (TTuple [TInt, TInt]) tyBool)
    ]
