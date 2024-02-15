module Toplevel (runMain) where

import AnfToClos
import ClosToUnty
import Data.Text          (Text)
import LamToAnf
import Parser
import Prettyprinter.Prec
import Prettyprinter.Util
import RawToLam

runMain :: Text -> IO ()
runMain inp = do
    raw_prog <- parseProg inp
    lam_prog <- r2lProg raw_prog
    let anf_prog = l2aProg lam_prog
    clos_prog <- a2cProg anf_prog
    let unty_prog = c2uProg clos_prog
    putDocW 60 $ pretty clos_prog
    putStrLn ""
    putDocW 60 $ pretty unty_prog
    putStrLn ""
