module Main (main) where

import Data.Time.Clock
import qualified Language.Haskell.TH.Syntax as TH
import Test.Tasty
import Test.Tasty.HUnit

testLift :: TestTree
testLift =
    testGroup
        "Lift instances"
        [ testCase "DiffTime" $ $$(TH.liftTyped (secondsToDiffTime 100)) @?= secondsToDiffTime 100
        , testCase "NominalDiffTime" $ $$(TH.liftTyped (secondsToNominalDiffTime 100)) @?= secondsToNominalDiffTime 100
        ]

tests :: TestTree
tests =
    testGroup
        "time-template"
        [ testLift
        ]

main :: IO ()
main = defaultMain tests
