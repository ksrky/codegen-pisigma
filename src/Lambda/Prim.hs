module Lambda.Prim (varTimes, varPlus, primDict) where

import Control.Lens.Operators
import Id
import Lambda

idTimes :: Id
idTimes = localId "*" & extern .~ True

varTimes :: Var
varTimes = (idTimes, TFun TInt (TFun TInt TInt))

idPlus :: Id
idPlus = localId "+" & extern .~ True

varPlus :: Var
varPlus = (idPlus, TFun TInt (TFun TInt TInt))

primDict :: [(String, Var)]
primDict = [("*", varTimes), ("+", varPlus)]
