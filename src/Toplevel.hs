module Toplevel (runMain) where

import AnfToClos
import ClosToUnty
import Data.Text          (Text)
import LamToAnf
import Parser
import Prettyprinter.Prec
import Prettyprinter.Util

runMain :: Text -> IO ()
runMain inp = do
    lam_prog <- parseProg "interactive" inp
    let anf_prog = l2aProg lam_prog
    let clos_prog = a2cProg anf_prog
    let unty_prog = c2uProg clos_prog
    putDocW 60 $ pretty clos_prog
    putStrLn ""
    putDocW 60 $ pretty unty_prog
    putStrLn ""
