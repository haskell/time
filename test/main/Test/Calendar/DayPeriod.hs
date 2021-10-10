module Test.Calendar.DayPeriod (
    testHasDays,
) where

import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

newtype WDay = MkWDay Day
    deriving (Eq, Show)

instance Arbitrary WDay where
    arbitrary = do
        (MkWYear y) <- arbitrary
        (MkWMonthOfYear m) <- arbitrary
        (MkWDayOfMonth d) <- arbitrary
        pure $ MkWDay $ YearMonthDay y m d

newtype WYear = MkWYear Year
    deriving (Eq, Show)

instance Arbitrary WYear where
    arbitrary = fmap MkWYear $ choose (-1000, 3000)

newtype WMonthOfYear = MkWMonthOfYear MonthOfYear
    deriving (Eq, Show)

instance Arbitrary WMonthOfYear where
    arbitrary = fmap MkWMonthOfYear $ choose (-5, 17)

newtype WMonth = MkWMonth Month
    deriving (Eq, Show)

instance Arbitrary WMonth where
    arbitrary = do
        (MkWYear y) <- arbitrary
        (MkWMonthOfYear m) <- arbitrary
        pure $ MkWMonth $ YearMonth y m

newtype WDayOfMonth = MkWDayOfMonth DayOfMonth
    deriving (Eq, Show)

instance Arbitrary WDayOfMonth where
    arbitrary = fmap MkWDayOfMonth $ choose (-5, 35)

testHasDays :: TestTree
testHasDays =
    testGroup
        "DayPeriod"
        [ testGroup "Day" testDay
        , testGroup "Month" testMonth
        , testGroup "Quarter" testQuarter
        , testGroup "Year" testYear
        ]

testDay :: [TestTree]
testDay =
    [ testProperty "periodFirstDay" $ \(MkWDay d) ->
        periodFirstDay d == d
    , testProperty "periodLastDay" $ \(MkWDay d) ->
        periodLastDay d == d
    ]

testMonth :: [TestTree]
testMonth =
    [ testProperty "periodFirstDay" $ \(MkWMonth my@(YearMonth y m)) ->
        periodFirstDay my == YearMonthDay y m 1
    , testGroup
        "periodLastDay"
        [ testCase "leap year" $
            periodLastDay (YearMonth 2024 February) @?= YearMonthDay 2024 February 29
        , testCase "regular year" $
            periodLastDay (YearMonth 2023 February) @?= YearMonthDay 2023 February 28
        ]
    ]

testQuarter :: [TestTree]
testQuarter =
    [ testGroup
        "periodFirstDay"
        [ testProperty "Q1" $ \(MkWYear y) ->
            periodFirstDay (YearQuarter y Q1) == YearMonthDay y January 1
        , testProperty "Q2" $ \(MkWYear y) ->
            periodFirstDay (YearQuarter y Q2) == YearMonthDay y April 1
        , testProperty "Q3" $ \(MkWYear y) ->
            periodFirstDay (YearQuarter y Q3) == YearMonthDay y July 1
        , testProperty "Q4" $ \(MkWYear y) ->
            periodFirstDay (YearQuarter y Q4) == YearMonthDay y October 1
        ]
    , testGroup
        "periodLastDay"
        [ testProperty "Q1" $ \(MkWYear y) ->
            periodLastDay (YearQuarter y Q1) == YearMonthDay y March 31
        , testProperty "Q2" $ \(MkWYear y) ->
            periodLastDay (YearQuarter y Q2) == YearMonthDay y June 30
        , testProperty "Q3" $ \(MkWYear y) ->
            periodLastDay (YearQuarter y Q3) == YearMonthDay y September 30
        , testProperty "Q4" $ \(MkWYear y) ->
            periodLastDay (YearQuarter y Q4) == YearMonthDay y December 31
        ]
    ]

testYear :: [TestTree]
testYear =
    [ testProperty "periodFirstDay" $ \(MkWYear y) ->
        periodFirstDay y == YearMonthDay y January 1
    , testProperty "periodLastDay" $ \(MkWYear y) ->
        periodLastDay y == YearMonthDay y December 31
    ]
