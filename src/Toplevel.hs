module Toplevel (runMain) where

import AnfClosure
import LambdaAnf
import RawLambda

import Data.Text          (Text)
import Parser
import Prettyprinter.Prec
import Prettyprinter.Util

runMain :: Text -> IO ()
runMain inp = do
    raw_prog <- parseProg inp
    lam_prog <- rawLambdaProgram raw_prog
    let anf_prog = lambdaAnfProgram lam_prog
    clos_prog <- anfClosureProgram anf_prog
    putDocW 60 $ pretty clos_prog
    putStrLn ""
