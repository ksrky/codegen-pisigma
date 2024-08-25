import Test.Tasty
import Tests.PisigmaTal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "PisigmaTal" [testsPisigmaTal]
    ]
