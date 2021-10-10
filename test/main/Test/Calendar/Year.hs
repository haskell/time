module Test.Calendar.Year (
    testYear,
) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

cbRoundTrip :: TestTree
cbRoundTrip = nameTest "CE/BCE" $ \(YearDay y _) -> case y of
    CommonEra n -> case y of
        BeforeCommonEra _ -> False
        _ -> n >= 1 && y == CommonEra n
    _ -> case y of
        BeforeCommonEra n -> n >= 1 && y == BeforeCommonEra n
        _ -> False

testYear :: TestTree
testYear =
    nameTest
        "Year"
        [ cbRoundTrip
        , nameTest "succ 1" $ assertEqual "" (BeforeCommonEra 1) $ succ $ BeforeCommonEra 2
        , nameTest "succ 2" $ assertEqual "" (CommonEra 1) $ succ $ BeforeCommonEra 1
        , nameTest "succ 3" $ assertEqual "" (CommonEra 2) $ succ $ CommonEra 1
        ]
