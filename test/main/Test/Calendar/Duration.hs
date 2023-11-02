module Test.Calendar.Duration (
    testDuration,
) where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)

data AddDiff = MkAddDiff
    { adName :: String
    , adAdd :: CalendarDiffDays -> Day -> Day
    , adDifference :: Day -> Day -> CalendarDiffDays
    , adFromYMD :: Integer -> MonthOfYear -> Int -> Day
    }

gregorianClip :: AddDiff
gregorianClip = MkAddDiff "gregorianClip" addGregorianDurationClip diffGregorianDurationClip fromGregorian

gregorianRollOver :: AddDiff
gregorianRollOver = MkAddDiff "gregorianRollOver" addGregorianDurationRollOver diffGregorianDurationRollOver fromGregorian

julianClip :: AddDiff
julianClip = MkAddDiff "julianClip" addJulianDurationClip diffJulianDurationClip fromJulian

julianRollOver :: AddDiff
julianRollOver = MkAddDiff "julianRollOver" addJulianDurationRollOver diffJulianDurationRollOver fromJulian

addDiffs :: [AddDiff]
addDiffs =
    [ gregorianClip
    , gregorianRollOver
    , julianClip
    , julianRollOver
    ]

testAddDiff :: AddDiff -> TestTree
testAddDiff MkAddDiff{..} = testProperty adName $ \day1 day2 ->
    adAdd (adDifference day2 day1) day1 == day2

testAddDiffs :: TestTree
testAddDiffs =
    testGroup
        "add-diff"
        $ fmap testAddDiff addDiffs

newtype Smallish = MkSmallish Integer deriving (Eq, Ord)

deriving newtype instance Show Smallish

instance Arbitrary Smallish where
    arbitrary = do
        b <- arbitrary
        n <- if b then choose (0, 60) else return 30
        return $ MkSmallish n

testPositiveDiff :: AddDiff -> TestTree
testPositiveDiff MkAddDiff{..} = testProperty adName $ \day1 (MkSmallish i) -> let
    day2 = addDays i day1
    r = adDifference day2 day1
    in property $ cdMonths r >= 0 && cdDays r >= 0

testPositiveDiffs :: TestTree
testPositiveDiffs =
    testGroup
        "positive-diff"
        $ fmap testPositiveDiff addDiffs

testSpecific :: AddDiff -> (Integer, MonthOfYear, Int) -> (Integer, MonthOfYear, Int) -> (Integer, Integer) -> TestTree
testSpecific MkAddDiff{..} (y2, m2, d2) (y1, m1, d1) (em, ed) = let
    day1 = adFromYMD y1 m1 d1
    day2 = adFromYMD y2 m2 d2
    expected = CalendarDiffDays em ed
    found = adDifference day2 day1
    in testCase (adName ++ ": " ++ show day2 ++ " - " ++ show day1) $ do
        assertEqual "add" day2 $ adAdd found day1
        assertEqual "diff" expected found

testSpecificPair :: (Integer, MonthOfYear, Int) -> (Integer, MonthOfYear, Int) -> (Integer, Integer) -> (Integer, Integer) -> TestTree
testSpecificPair day2 day1 clipD rollD =
    testGroup
        (show day2 ++ " - " ++ show day1)
        [ testSpecific gregorianClip day2 day1 clipD
        , testSpecific gregorianRollOver day2 day1 rollD
        , testSpecific julianClip day2 day1 clipD
        , testSpecific julianRollOver day2 day1 rollD
        ]

testSpecifics :: TestTree
testSpecifics =
    testGroup
        "specific"
        [ testSpecificPair (2017, April, 07) (2017, April, 07) (0, 0) (0, 0)
        , testSpecific gregorianClip (2017, April, 07) (2017, April, 01) (0, 6)
        , testSpecific gregorianClip (2017, April, 01) (2017, April, 07) (0, -6)
        , testSpecific gregorianClip (2017, April, 07) (2017, February, 01) (2, 6)
        , testSpecific gregorianClip (2017, February, 01) (2017, April, 07) (-2, -6)
        , testSpecificPair (2000, March, 01) (2000, January, 30) (1, 1) (1, 0)
        , testSpecificPair (2001, March, 01) (2001, January, 30) (1, 1) (0, 30)
        , testSpecificPair (2001, March, 01) (2000, January, 30) (13, 1) (12, 30)
        , testSpecificPair (2000, March, 01) (2000, January, 31) (1, 1) (0, 30)
        , testSpecificPair (2001, March, 01) (2001, January, 31) (1, 1) (0, 29)
        , testSpecificPair (2001, March, 01) (2000, January, 31) (13, 1) (12, 29)
        , testSpecificPair (2001, October, 01) (2001, August, 31) (1, 1) (1, 0)
        ]

testDuration :: TestTree
testDuration = testGroup "CalendarDiffDays" [testAddDiffs, testPositiveDiffs, testSpecifics]
