module PisigmaTal.Toplevel (compileToTal, runProgram) where

import Data.Text              (Text)
import PisigmaTal.AnfClosure
import PisigmaTal.ClosureTal
import PisigmaTal.LambdaAnf
import PisigmaTal.Parser
import PisigmaTal.RawLambda
import PisigmaTal.Tal         as Tal
import PisigmaTal.Tal.Builder qualified as Tal
import Tal.Context            qualified as Tal
import Tal.Interpreter        qualified as Tal

compileToTal :: Text -> IO Tal.Program
compileToTal inp = do
    raw_prog     <- parseProgram inp
    lambda_prog  <- rawLambdaProgram raw_prog
    let anf_prog =  lambdaAnfProgram lambda_prog
    closure_prog <- anfClosureProgram anf_prog
    closureTalProgram closure_prog

runProgram :: Text -> IO ()
runProgram inp = do
    prog <- compileToTal inp
    ctx <- Tal.createContext
    Tal.buildProgram ctx prog
    rctx <- Tal.createRuntimeContext ctx
    Tal.steps rctx
