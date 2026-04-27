module Test.Format.Duration (
    testISO8601Duration,
) where

import Data.Time.Format.ISO8601
import Data.Time.Format.ISO8601.Duration
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

roundTrip :: String -> TestTree
roundTrip s =
    nameTest s $
        case formatParseM durationFormat s :: Maybe Duration of
            Nothing -> assertFailure $ "did not parse: " ++ s
            Just d ->
                case formatShowM durationFormat d of
                    Nothing -> assertFailure $ "did not show: " ++ show d
                    Just s' -> assertEqual "round-trip" s s'

testRoundTrip :: TestTree
testRoundTrip =
    nameTest
        "round-trip"
        [ roundTrip "P1Y"
        , roundTrip "P12M"
        , roundTrip "P1W"
        , roundTrip "P7D"
        , roundTrip "PT1H"
        , roundTrip "PT60M"
        , roundTrip "PT3600S"
        , roundTrip "P1Y2M3D"
        , roundTrip "P1Y2M3DT4H5M6S"
        , roundTrip "PT1.5S"
        , roundTrip "-P1Y"
        , roundTrip "-PT1H30M"
        , roundTrip "P0D"
        ]

testRejectInvalid :: TestTree
testRejectInvalid =
    nameTest
        "reject invalid"
        [ nameTest "empty P" $
            assertEqual "" Nothing (formatParseM durationFormat "P" :: Maybe Duration)
        , nameTest "empty PT" $
            assertEqual "" Nothing (formatParseM durationFormat "PT" :: Maybe Duration)
        , nameTest "P1WD mixed week+day" $
            assertEqual "" Nothing (formatParseM durationFormat "P1W2D" :: Maybe Duration)
        ]

testToCalendarDiffTime :: TestTree
testToCalendarDiffTime =
    let
        -- Demonstrates that going through CalendarDiffTime collapses
        -- distinct lexical forms into a single canonical one (the
        -- exact reason structure-preserving Duration is needed).
        check :: String -> String -> TestTree
        check src expected =
            nameTest (src ++ " -> CalendarDiffTime " ++ expected) $
                case formatParseM durationFormat src :: Maybe Duration of
                    Nothing -> assertFailure $ "parse failed: " ++ src
                    Just d -> assertEqual "" expected (iso8601Show (toCalendarDiffTime d))
    in
        nameTest
            "to CalendarDiffTime (lossy by design)"
            [ check "P1Y" "P1Y"
            , check "P12M" "P1Y"
            , check "P1W" "P7D"
            , check "P7D" "P7D"
            , check "PT1H" "PT1H"
            , check "PT60M" "PT1H"
            , check "PT3600S" "PT1H"
            ]

testISO8601Duration :: TestTree
testISO8601Duration =
    nameTest
        "Duration"
        [ testRoundTrip
        , testRejectInvalid
        , testToCalendarDiffTime
        ]
