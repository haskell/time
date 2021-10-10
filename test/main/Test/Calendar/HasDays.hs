module Test.Calendar.HasDays (
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
        "HasDays"
        [ testGroup "Day" testDay
        , testGroup "Month" testMonth
        , testGroup "Quarter" testQuarter
        , testGroup "Year" testYear
        ]

testDay :: [TestTree]
testDay =
    [ testProperty "firstDayOf" $ \(MkWDay d) ->
        firstDayOf d == d
    , testProperty "lastDayOf" $ \(MkWDay d) ->
        lastDayOf d == d
    ]

testMonth :: [TestTree]
testMonth =
    [ testProperty "firstDayOf" $ \(MkWMonth my@(YearMonth y m)) ->
        firstDayOf my == YearMonthDay y m 1
    , testGroup
        "lastDayOf"
        [ testCase "leap year" $
            lastDayOf (YearMonth 2024 February) @?= YearMonthDay 2024 February 29
        , testCase "regular year" $
            lastDayOf (YearMonth 2023 February) @?= YearMonthDay 2023 February 28
        ]
    ]

testQuarter :: [TestTree]
testQuarter =
    [ testGroup
        "firstDayOf"
        [ testProperty "Q1" $ \(MkWYear y) ->
            firstDayOf (YearQuarter y Q1) == YearMonthDay y January 1
        , testProperty "Q2" $ \(MkWYear y) ->
            firstDayOf (YearQuarter y Q2) == YearMonthDay y April 1
        , testProperty "Q3" $ \(MkWYear y) ->
            firstDayOf (YearQuarter y Q3) == YearMonthDay y July 1
        , testProperty "Q4" $ \(MkWYear y) ->
            firstDayOf (YearQuarter y Q4) == YearMonthDay y October 1
        ]
    , testGroup
        "lastDayOf"
        [ testProperty "Q1" $ \(MkWYear y) ->
            lastDayOf (YearQuarter y Q1) == YearMonthDay y March 31
        , testProperty "Q2" $ \(MkWYear y) ->
            lastDayOf (YearQuarter y Q2) == YearMonthDay y June 30
        , testProperty "Q3" $ \(MkWYear y) ->
            lastDayOf (YearQuarter y Q3) == YearMonthDay y September 30
        , testProperty "Q4" $ \(MkWYear y) ->
            lastDayOf (YearQuarter y Q4) == YearMonthDay y December 31
        ]
    ]

testYear :: [TestTree]
testYear =
    [ testProperty "firstDayOf" $ \(MkWYear y) ->
        firstDayOf y == YearMonthDay y January 1
    , testProperty "lastDayOf" $ \(MkWYear y) ->
        lastDayOf y == YearMonthDay y December 31
    ]
