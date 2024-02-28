module Toplevel (runMain) where

import AnfClosure
import ClosUnty
import Data.Text          (Text)
import LambdaAnf
import Parser
import Prettyprinter.Prec
import Prettyprinter.Util
import RawLambda

runMain :: Text -> IO ()
runMain inp = do
    raw_prog <- parseProg inp
    lam_prog <- rawLambdaProgram raw_prog
    let anf_prog = lambdaAnfProgram lam_prog
    clos_prog <- anfClosureProgram anf_prog
    let unty_prog = c2uProg clos_prog
    putDocW 60 $ pretty clos_prog
    putStrLn ""
    putDocW 60 $ pretty unty_prog
    putStrLn ""
