module Test.LocalTime.TimeZone (
    testTimeZone,
) where

import Data.Time
import System.Environment (setEnv)
import Test.Tasty
import Test.Tasty.HUnit

testTimeZone :: TestTree
testTimeZone =
    testCase "getTimeZone respects TZ env var" $ do
        let
            epoch = UTCTime (ModifiedJulianDay 57000) 0
        setEnv "TZ" "UTC+0"
        zone1 <- getTimeZone epoch
        setEnv "TZ" "EST+5"
        zone2 <- getTimeZone epoch
        assertBool "zone not changed" $ zone1 /= zone2
