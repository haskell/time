module Test.Clock.Conversion (
    testClockConversion,
) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Test.Tasty
import Test.Tasty.HUnit

testClockConversion :: TestTree
testClockConversion =
    testGroup "clock conversion" $
        let
            testPair :: (SystemTime, UTCTime) -> TestTree
            testPair (st, ut) =
                testGroup (show ut) $
                    [ testCase "systemToUTCTime" $ assertEqual (show ut) ut $ systemToUTCTime st
                    , testCase "utcToSystemTime" $ assertEqual (show ut) st $ utcToSystemTime ut
                    ]
            testPOSIXTime :: (SystemTime, POSIXTime) -> TestTree
            testPOSIXTime (st, pt) =
                testGroup
                    (show st)
                    [ testCase "expected" $
                        assertEqual (show st) pt $ systemToPOSIXTime st
                    , testCase "matches systemToUTCTime" $
                        assertEqual
                            (show st)
                            (utcTimeToPOSIXSeconds $ systemToUTCTime st)
                            (systemToPOSIXTime st)
                    ]
        in
            [ testPair (MkSystemTime 0 0, UTCTime systemEpochDay 0)
            , testPair (MkSystemTime 86399 0, UTCTime systemEpochDay 86399)
            , testPair (MkSystemTime 86399 999999999, UTCTime systemEpochDay 86399.999999999)
            , testPair (MkSystemTime 86399 1000000000, UTCTime systemEpochDay 86400)
            , testPair (MkSystemTime 86399 1999999999, UTCTime systemEpochDay 86400.999999999)
            , testPair (MkSystemTime 86400 0, UTCTime (succ systemEpochDay) 0)
            , testGroup
                "systemToPOSIXTime"
                [ testPOSIXTime (MkSystemTime 0 0, 0)
                , testPOSIXTime (MkSystemTime 86399 999999999, 86399.999999999)
                , testPOSIXTime (MkSystemTime 86399 1000000000, 86400)
                , testPOSIXTime (MkSystemTime 86399 1500000000, 86400)
                , testPOSIXTime (MkSystemTime 86399 1999999999, 86400)
                , testPOSIXTime (MkSystemTime 86400 0, 86400)
                ]
            ]
