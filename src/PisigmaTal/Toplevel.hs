module PisigmaTal.Toplevel (compileToTal) where

import Data.Text             (Text)
import PisigmaTal.AnfClosure
import PisigmaTal.ClosureTal
import PisigmaTal.LambdaAnf
import PisigmaTal.Parser
import PisigmaTal.RawLambda
import PisigmaTal.Tal        as Tal

compileToTal :: Text -> IO Tal.Program
compileToTal inp = do
    raw_prog     <- parseProgram inp
    lambda_prog  <- rawLambdaProgram raw_prog
    let anf_prog =  lambdaAnfProgram lambda_prog
    closure_prog <- anfClosureProgram anf_prog
    closureTalProgram closure_prog
