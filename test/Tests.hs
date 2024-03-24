import Test.Tasty
import Tests.PisigmaTal
import Tests.Tal.Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "PisigmaTal" [testsPisigmaTal]
    , testGroup "Tal" [interpreterTest]
    ]
