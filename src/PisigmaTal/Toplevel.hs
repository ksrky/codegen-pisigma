module Toplevel (compileToTal) where

import AnfClosure
import ClosureAlloc
import LambdaAnf
import Parser
import RawLambda

import AllocTal
import Data.Text    (Text)

compileToTal :: Text -> IO ()
compileToTal inp = do
    raw_prog     <- parseProgram inp
    lambda_prog  <- rawLambdaProgram raw_prog
    let anf_prog =  lambdaAnfProgram lambda_prog
    closure_prog <- anfClosureProgram anf_prog
    alloc_prog   <- closureAllocProgram closure_prog
    _tal_prog    <- allocTalProgram alloc_prog
    return ()
