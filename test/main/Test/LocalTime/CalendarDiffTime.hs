module Test.LocalTime.CalendarDiffTime (
    testCalendarDiffTime,
) where

import Data.Time
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

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
        ]
