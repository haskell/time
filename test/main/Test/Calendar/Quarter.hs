module Test.Calendar.Quarter (
    testQuarter,
) where

import Data.Time.Calendar
import Data.Time.Calendar.Quarter
import Test.Tasty
import Test.Tasty.QuickCheck

newtype WYear = MkWYear Year
    deriving (Eq, Show)

instance Arbitrary WYear where
    arbitrary = fmap MkWYear $ choose (-1000, 3000)

newtype Q1Month = MkQ1Month MonthOfYear
    deriving (Eq, Show)

instance Arbitrary Q1Month where
    arbitrary = fmap MkQ1Month $ choose (January, March)

newtype Q2Month = MkQ2Month MonthOfYear
    deriving (Eq, Show)

instance Arbitrary Q2Month where
    arbitrary = fmap MkQ2Month $ choose (April, June)

newtype Q3Month = MkQ3Month MonthOfYear
    deriving (Eq, Show)

instance Arbitrary Q3Month where
    arbitrary = fmap MkQ3Month $ choose (July, September)

newtype Q4Month = MkQ4Month MonthOfYear
    deriving (Eq, Show)

instance Arbitrary Q4Month where
    arbitrary = fmap MkQ4Month $ choose (October, December)

testQuarter :: TestTree
testQuarter =
    testGroup
        "Quarter"
        [ testGroup
            "allQuarterByDay"
            [ testProperty "Q1" $ \(MkWYear y, MkQ1Month m) ->
                allQuarterByDay (YearMonthDay y m 1) == (YearMonthDay y January 1, YearMonthDay y March 31)
            , testProperty "Q2" $ \(MkWYear y, MkQ2Month m) ->
                allQuarterByDay (YearMonthDay y m 1) == (YearMonthDay y April 1, YearMonthDay y June 30)
            , testProperty "Q3" $ \(MkWYear y, MkQ3Month m) ->
                allQuarterByDay (YearMonthDay y m 1) == (YearMonthDay y July 1, YearMonthDay y September 30)
            , testProperty "Q4" $ \(MkWYear y, MkQ4Month m) ->
                allQuarterByDay (YearMonthDay y m 1) == (YearMonthDay y October 1, YearMonthDay y December 31)
            ]
        , testGroup
            "allQuarter"
            [ testProperty "Q1" $ \(MkWYear y) ->
                allQuarter (YearQuarter y Q1) == (YearMonthDay y January 1, YearMonthDay y March 31)
            , testProperty "Q2" $ \(MkWYear y) ->
                allQuarter (YearQuarter y Q2) == (YearMonthDay y April 1, YearMonthDay y June 30)
            , testProperty "Q3" $ \(MkWYear y) ->
                allQuarter (YearQuarter y Q3) == (YearMonthDay y July 1, YearMonthDay y September 30)
            , testProperty "Q4" $ \(MkWYear y) ->
                allQuarter (YearQuarter y Q4) == (YearMonthDay y October 1, YearMonthDay y December 31)
            ]
        ]
