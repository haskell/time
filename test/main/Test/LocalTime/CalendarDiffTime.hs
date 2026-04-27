module Test.LocalTime.CalendarDiffTime (
    testCalendarDiffTime,
) where

import Data.Fixed
import Data.Time
import Test.AddDiff
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

utcClip :: AddDiff CalendarDiffTime UTCTime
utcClip = MkAddDiff "utcClip" addUTCDurationClip diffUTCDurationClip

utcRollOver :: AddDiff CalendarDiffTime UTCTime
utcRollOver = MkAddDiff "utcRollOver" addUTCDurationRollOver diffUTCDurationRollOver

localClip :: AddDiff CalendarDiffTime LocalTime
localClip = MkAddDiff "localClip" addLocalDurationClip diffLocalDurationClip

localRollOver :: AddDiff CalendarDiffTime LocalTime
localRollOver = MkAddDiff "localRollOver" addLocalDurationRollOver diffLocalDurationRollOver

testAddDiffs :: TestTree
testAddDiffs =
    testGroup
        "add-diff"
        [ testGroup "UTC" $ fmap testAddDiff [utcClip, utcRollOver]
        , testGroup "LocalTime" $ fmap testAddDiff [localClip, localRollOver]
        ]

utcTime :: Integer -> Int -> Int -> DiffTime -> UTCTime
utcTime y m d = UTCTime (YearMonthDay y m d)

localTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
localTime y m d h minute s = LocalTime (YearMonthDay y m d) $ TimeOfDay h minute s

testSpecifics :: TestTree
testSpecifics =
    testGroup
        "specific"
        [ testGroup
            "add"
            [ testCase "UTC clip" $
                assertEqual
                    "Jan 31 + 1 month clips to Feb 28"
                    (utcTime 2001 2 28 0)
                    (addUTCDurationClip (CalendarDiffTime 1 0) $ utcTime 2001 1 31 0)
            , testCase "UTC rollover" $
                assertEqual
                    "Jan 31 + 1 month rolls over to Mar 3"
                    (utcTime 2001 3 3 0)
                    (addUTCDurationRollOver (CalendarDiffTime 1 0) $ utcTime 2001 1 31 0)
            , testCase "LocalTime clip" $
                assertEqual
                    "Jan 31 + 1 month clips to Feb 28"
                    (localTime 2001 2 28 0 0 0)
                    (addLocalDurationClip (CalendarDiffTime 1 0) $ localTime 2001 1 31 0 0 0)
            , testCase "LocalTime rollover" $
                assertEqual
                    "Jan 31 + 1 month rolls over to Mar 3"
                    (localTime 2001 3 3 0 0 0)
                    (addLocalDurationRollOver (CalendarDiffTime 1 0) $ localTime 2001 1 31 0 0 0)
            , testCase "time part" $
                assertEqual
                    "the time part can carry into the next day"
                    (localTime 2001 2 1 0 0 1)
                    (addLocalDurationClip (CalendarDiffTime 0 2) $ localTime 2001 1 31 23 59 59)
            ]
        , testGroup
            "diff"
            [ testCase "UTC clip" $
                assertEqual
                    "Mar 1 - Jan 30 clips as 1 month and 1 day"
                    (CalendarDiffTime 1 nominalDay)
                    (diffUTCDurationClip (utcTime 2001 3 1 0) $ utcTime 2001 1 30 0)
            , testCase "UTC rollover" $
                assertEqual
                    "Mar 1 - Jan 30 rolls over as 30 days"
                    (CalendarDiffTime 0 $ 30 * nominalDay)
                    (diffUTCDurationRollOver (utcTime 2001 3 1 0) $ utcTime 2001 1 30 0)
            , testCase "LocalTime clip" $
                assertEqual
                    "Mar 1 - Jan 30 clips as 1 month and 1 day"
                    (CalendarDiffTime 1 nominalDay)
                    (diffLocalDurationClip (localTime 2001 3 1 0 0 0) $ localTime 2001 1 30 0 0 0)
            , testCase "LocalTime rollover" $
                assertEqual
                    "Mar 1 - Jan 30 rolls over as 30 days"
                    (CalendarDiffTime 0 $ 30 * nominalDay)
                    (diffLocalDurationRollOver (localTime 2001 3 1 0 0 0) $ localTime 2001 1 30 0 0 0)
            ]
        ]

testReadShowExact :: (Read a, Show a, Eq a) => String -> a -> TestTree
testReadShowExact t v =
    nameTest
        t
        [ nameTest "show" $ assertEqual "show" t $ show v
        , nameTest "read" $ assertEqual "read" v $ read t
        ]

testCalendarDiffTime :: TestTree
testCalendarDiffTime =
    nameTest
        "CalendarDiffTime"
        [ testReadShowExact "P0D" $ CalendarDiffTime 0 0
        , testReadShowExact "P1DT1S" $ CalendarDiffTime 0 $ secondsToNominalDiffTime 86401
        , testReadShowExact "P-1DT1S" $ CalendarDiffTime 0 $ secondsToNominalDiffTime $ negate 86399
        , testReadShowExact "P-1D" $ CalendarDiffTime 0 $ secondsToNominalDiffTime $ negate 86400
        , testReadShowExact "P-2DT23H59M59S" $ CalendarDiffTime 0 $ secondsToNominalDiffTime $ negate 86401
        , testReadShowExact "P1M-1DT1S" $ CalendarDiffTime 1 $ secondsToNominalDiffTime $ negate 86399
        , testReadShowExact "P1M-1D" $ CalendarDiffTime 1 $ secondsToNominalDiffTime $ negate 86400
        , testReadShowExact "P1M-2DT23H59M59S" $ CalendarDiffTime 1 $ secondsToNominalDiffTime $ negate 86401
        , testReadShowExact "P-1Y-1M-1DT1S" $ CalendarDiffTime (-13) $ secondsToNominalDiffTime $ negate 86399
        , testReadShowExact "P-1Y-1M-1D" $ CalendarDiffTime (-13) $ secondsToNominalDiffTime $ negate 86400
        , testReadShowExact "P-1Y-1M-2DT23H59M59S" $ CalendarDiffTime (-13) $ secondsToNominalDiffTime $ negate 86401
        , testAddDiffs
        , testSpecifics
        ]
