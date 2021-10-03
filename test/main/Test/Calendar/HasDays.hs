module Test.Calendar.HasDays (
    testHasDays,
) where

import Data.Time.Calendar
import Data.Time.Calendar.Quarter
import Test.Tasty
import Test.Tasty.QuickCheck

newtype WYear = MkWYear Year
    deriving (Eq, Show)

instance Arbitrary WYear where
    arbitrary = fmap MkWYear $ choose (-1000, 3000)

testHasDays :: TestTree
testHasDays =
    testGroup
        "HasDays"
        [ testGroup
            "firstDay"
            [ testProperty "Q1" $ \(MkWYear y) ->
                firstDay (YearQuarter y Q1) == YearMonthDay y January 1
            , testProperty "Q2" $ \(MkWYear y) ->
                firstDay (YearQuarter y Q2) == YearMonthDay y April 1
            , testProperty "Q3" $ \(MkWYear y) ->
                firstDay (YearQuarter y Q3) == YearMonthDay y July 1
            , testProperty "Q4" $ \(MkWYear y) ->
                firstDay (YearQuarter y Q4) == YearMonthDay y October 1
            ]
        , testGroup
            "lastDay"
            [ testProperty "Q1" $ \(MkWYear y) ->
                lastDay (YearQuarter y Q1) == YearMonthDay y March 31
            , testProperty "Q2" $ \(MkWYear y) ->
                lastDay (YearQuarter y Q2) == YearMonthDay y June 30
            , testProperty "Q3" $ \(MkWYear y) ->
                lastDay (YearQuarter y Q3) == YearMonthDay y September 30
            , testProperty "Q4" $ \(MkWYear y) ->
                lastDay (YearQuarter y Q4) == YearMonthDay y December 31
            ]
        ]
