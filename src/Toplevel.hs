module Toplevel (runMain) where

import AnfToClos
import Data.Text                 (Text)
import LamToAnf
import Parser
import Prettyprinter.Prec
import Prettyprinter.Render.Text

runMain :: Text -> IO ()
runMain inp = do
    lam_prog <- parseProg "interactive" inp
    let anf_prog = l2aProg lam_prog
    let clos_prog = a2cProg anf_prog
    putDoc $ pretty clos_prog
    putStrLn ""
