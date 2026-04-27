{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Format.Duration (
    testISO8601Duration,
) where

import Control.DeepSeq (deepseq)
import Data.Time hiding
    ( addLocalDurationClip
    , addLocalDurationRollOver
    , addUTCDurationClip
    , addUTCDurationRollOver
    , diffLocalDurationClip
    , diffLocalDurationRollOver
    , diffUTCDurationClip
    , diffUTCDurationRollOver
    )
import Data.Time.Format.ISO8601
import Data.Time.Format.ISO8601.Duration
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

parseD :: String -> Duration
parseD s = case formatParseM durationFormat s :: Maybe Duration of
    Just d -> d
    Nothing -> error $ "parse failed: " ++ s

showD :: Duration -> String
showD d = case formatShowM durationFormat d of
    Just s -> s
    Nothing -> error $ "show failed: " ++ show d

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
    let
        rej name s =
            nameTest name $
                assertEqual ("" ++ s) Nothing (formatParseM durationFormat s :: Maybe Duration)
    in
        nameTest
            "reject invalid (XSD 1.1 sec.3.3.6.2)"
            [ rej "empty P" "P"
            , rej "empty PT" "PT"
            , rej "trailing T (P1YT) per .*[^T] rule" "P1YT"
            , rej "trailing T (P1Y2MT)" "P1Y2MT"
            , rej "P1WD mixed week+day" "P1W2D"
            , rej "leading + (XSD prod. [15] only allows '-'?)" "+P1Y"
            , rej "decimal on years (XSD prod. [11] limits decimals to S)"
                "P1.5Y"
            , rej "decimal on months" "P1.5M"
            , rej "decimal on days" "P1.5D"
            , rej "decimal on hours" "PT1.5H"
            , rej "decimal on minutes" "PT1.5M"
            ]

testXSDLexicalEdges :: TestTree
testXSDLexicalEdges =
    let
        accepts name s expected =
            nameTest name $
                case formatParseM durationFormat s :: Maybe Duration of
                    Just d -> assertEqual "" expected (showD d)
                    Nothing -> assertFailure $ "did not parse: " ++ s
    in
        nameTest
            "XSD 1.1 sec.3.3.6.2 lexical edges"
            [ accepts "lexical fields unbounded: P400M parses" "P400M" "P400M"
            , accepts "PT72H parses (canonical normalises later)" "PT72H" "PT72H"
            , accepts "P999999999Y parses (bounded=false sec.3.3.6.3)"
                "P999999999Y"
                "P999999999Y"
            , nameTest "P400M normalises to P33Y4M" $
                assertEqual "" "P33Y4M" (showD (normalizeDuration (parseD "P400M")))
            , nameTest "PT72H normalises to P3D" $
                assertEqual "" "P3D" (showD (normalizeDuration (parseD "PT72H")))
            ]

testFractionalSeconds :: TestTree
testFractionalSeconds =
    let
        rt s = roundTrip s
    in
        nameTest
            "fractional seconds (XSD sec.4.3.2: >=3 decimals must work)"
            [ rt "PT0.5S"
            , rt "PT0.001S"
            , rt "PT0.000001S"
            , rt "PT123456.789S"
            , nameTest "PT90.5S normalises to PT1M30.5S" $
                assertEqual "" "PT1M30.5S" (showD (normalizeDuration (parseD "PT90.5S")))
            , nameTest "decimal preserved through Semigroup" $
                let a = parseD "PT0.25S"
                    b = parseD "PT0.25S"
                in assertEqual "" "PT0.5S" (showD (a <> b))
            ]

testXSDCanonicalBounds :: TestTree
testXSDCanonicalBounds =
    let
        check :: String -> String -> TestTree
        check src expected =
            nameTest (src ++ " ~> " ++ expected) $
                assertEqual "" expected (showD (normalizeDuration (parseD src)))
    in
        nameTest
            "XSD sec.E.2 canonical bounds"
            [ check "PT24H" "P1D"      -- hours bounded [0,23]
            , check "PT25H" "P1DT1H"
            , check "PT60M" "PT1H"     -- minutes bounded [0,59]
            , check "PT61M" "PT1H1M"
            , check "PT60S" "PT1M"     -- seconds bounded [0, 60)
            , check "PT119S" "PT1M59S"
            , check "P11M" "P11M"      -- months bounded [0, 11]
            , check "P12M" "P1Y"
            , check "P23M" "P1Y11M"
            , check "P24M" "P2Y"
            , check "P365D" "P365D"    -- days unbounded
            , check "P366D" "P366D"
            ]

testXSDSignDecomposition :: TestTree
testXSDSignDecomposition =
    let
        d = parseD "-P1Y2M3DT4H5M6S"
    in
        nameTest
            "single sign covers all axes (XSD sec.3.3.6.1)"
            [ nameTest "round-trips" $
                assertEqual "" "-P1Y2M3DT4H5M6S" (showD d)
            , nameTest "all months negated together" $
                assertEqual "" (-14) (ctMonths (toCalendarDiffTime d))
            , nameTest "all time components negated together" $
                assertBool "" (ctTime (toCalendarDiffTime d) < 0)
            , nameTest "negate by scale (-1) returns positive" $
                assertEqual "" "P1Y2M3DT4H5M6S" (showD (scaleDuration (-1) d))
            ]

testAdditionNonCommutative :: TestTree
testAdditionNonCommutative =
    let
        u = UTCTime (fromGregorian 2024 1 30) 0
        d_day = parseD "P1D"
        d_mo = parseD "P1M"
    in
        nameTest
            "(d+P1D)+P1M /= (d+P1M)+P1D in general (XSD sec.E.3.3 note)"
            -- Jan 30 + 1D = Jan 31; + 1M clip = Feb 29 (leap)
            -- Jan 30 + 1M = Feb 29 (leap clip); + 1D = Mar 1
            -- These differ — demonstrating non-commutativity.
            [ nameTest "diverges on Jan 30, 2024" $
                let r1 = addUTCDurationClip d_mo (addUTCDurationClip d_day u)
                    r2 = addUTCDurationClip d_day (addUTCDurationClip d_mo u)
                in assertBool ("expected r1 /= r2; got " ++ show r1 ++ " vs " ++ show r2)
                    (r1 /= r2)
            ]

testXsdRejectsAlternative :: TestTree
testXsdRejectsAlternative =
    nameTest
        "xsdDurationFormat rejects ISO 8601 alt form (sec.4.4.3.3)"
        -- Alternative form Pyyyy-mm-ddThh:mm:ss is in ISO 8601:2004
        -- but not in XSD 1.1 grammar; durationFormat doesn't accept
        -- it either (this format only handles the designator form),
        -- so this test confirms both reject it.
        [ nameTest "durationFormat rejects P0001-02-03" $
            assertEqual ""
                Nothing
                (formatParseM durationFormat "P0001-02-03" :: Maybe Duration)
        , nameTest "xsdDurationFormat rejects P0001-02-03" $
            assertEqual ""
                Nothing
                (formatParseM xsdDurationFormat "P0001-02-03" :: Maybe Duration)
        ]

testToCalendarDiffTime :: TestTree
testToCalendarDiffTime =
    let
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

testNormalize :: TestTree
testNormalize =
    let
        check :: String -> String -> TestTree
        check src expected =
            nameTest (src ++ " ~> " ++ expected) $
                assertEqual "" expected (showD (normalizeDuration (parseD src)))
        idempotent s =
            nameTest (s ++ " idempotent") $
                let n = normalizeDuration (parseD s)
                in assertEqual "" n (normalizeDuration n)
    in
        nameTest
            "normalizeDuration (XSD 1.1 sec.E.2)"
            [ check "P12M" "P1Y"
            , check "P13M" "P1Y1M"
            , check "P1W" "P7D"
            , check "PT60S" "PT1M"
            , check "PT3600S" "PT1H"
            , check "PT86400S" "P1D"
            , check "PT90.5S" "PT1M30.5S"
            , check "P30D" "P30D" -- D does NOT carry into M
            , check "-P12M" "-P1Y"
            , check "P0D" "P0D"
            , idempotent "P1Y2M3DT4H5M6.7S"
            , idempotent "P12M"
            , idempotent "P1W"
            , idempotent "PT3600S"
            ]

testSemigroup :: TestTree
testSemigroup =
    let
        check :: String -> String -> String -> TestTree
        check a b expected =
            nameTest (a ++ " <> " ++ b ++ " = " ++ expected) $
                assertEqual "" expected (showD (parseD a <> parseD b))
        identity s =
            nameTest (s ++ " <> mempty") $
                assertEqual "" (normalizeDuration (parseD s)) (parseD s <> mempty)
    in
        nameTest
            "Semigroup / Monoid (XSD sec.3.3.6.1)"
            [ check "P1Y" "P12M" "P2Y"
            , check "P1W" "P2D" "P9D"
            , check "PT30M" "PT30M" "PT1H"
            , check "PT45M" "PT15M" "PT1H"
            , check "P1Y" "PT1H" "P1YT1H"
            , identity "P1Y2M3DT4H5M6S"
            , identity "P1W"
            , nameTest "associative on a sample" $
                let a = parseD "P1Y2M"
                    b = parseD "P3DT4H"
                    c = parseD "PT5M6S"
                in assertEqual "" ((a <> b) <> c) (a <> (b <> c))
            ]

testScale :: TestTree
testScale =
    let
        check :: Integer -> String -> String -> TestTree
        check k src expected =
            nameTest ("scale " ++ show k ++ " " ++ src ++ " = " ++ expected) $
                assertEqual "" expected (showD (scaleDuration k (parseD src)))
    in
        nameTest
            "scaleDuration"
            [ check 2 "P1Y" "P2Y"
            , check 3 "P1M" "P3M"
            , check 2 "PT30M" "PT1H"
            , check 0 "P1Y2M3DT4H5M6S" "P0D"
            , check (-1) "P1Y" "-P1Y"
            ]

testArithmetic :: TestTree
testArithmetic =
    let
        u0 = UTCTime (fromGregorian 2024 1 15) 0
        l0 = LocalTime (fromGregorian 2024 1 15) midnight
        d1Y = parseD "P1Y"
        d1M = parseD "P1M"
        -- Jan 31 + P1M: clip pins to Feb 29 (leap), rollover gives Mar 2.
        u_jan31 = UTCTime (fromGregorian 2024 1 31) 0
        l_jan31 = LocalTime (fromGregorian 2024 1 31) midnight
    in
        nameTest
            "add/diff arithmetic"
            [ nameTest "addUTCDurationClip P1Y to 2024-01-15" $
                assertEqual "" (UTCTime (fromGregorian 2025 1 15) 0) (addUTCDurationClip d1Y u0)
            , nameTest "addUTCDurationRollOver P1Y to 2024-01-15" $
                assertEqual "" (UTCTime (fromGregorian 2025 1 15) 0) (addUTCDurationRollOver d1Y u0)
            , nameTest "addLocalDurationClip P1M to 2024-01-15" $
                assertEqual "" (LocalTime (fromGregorian 2024 2 15) midnight) (addLocalDurationClip d1M l0)
            , nameTest "addLocalDurationRollOver P1M to 2024-01-15" $
                assertEqual "" (LocalTime (fromGregorian 2024 2 15) midnight) (addLocalDurationRollOver d1M l0)
            , nameTest "Clip vs RollOver diverge on Jan 31 + P1M (UTC)" $
                let clipped = addUTCDurationClip d1M u_jan31
                    rolled = addUTCDurationRollOver d1M u_jan31
                in do
                    assertEqual "clip pins to Feb 29" (UTCTime (fromGregorian 2024 2 29) 0) clipped
                    assertEqual "rollover advances past Feb" (UTCTime (fromGregorian 2024 3 2) 0) rolled
            , nameTest "Clip vs RollOver diverge on Jan 31 + P1M (Local)" $
                let clipped = addLocalDurationClip d1M l_jan31
                    rolled = addLocalDurationRollOver d1M l_jan31
                in do
                    assertEqual "clip" (LocalTime (fromGregorian 2024 2 29) midnight) clipped
                    assertEqual "rollover" (LocalTime (fromGregorian 2024 3 2) midnight) rolled
            , nameTest "diffUTCDurationClip 2025-01-15 - 2024-01-15 = P1Y" $
                let u1 = UTCTime (fromGregorian 2025 1 15) 0
                in assertEqual "" "P1Y" (showD (diffUTCDurationClip u1 u0))
            , nameTest "diffUTCDurationRollOver 2025-01-15 - 2024-01-15 = P1Y" $
                let u1 = UTCTime (fromGregorian 2025 1 15) 0
                in assertEqual "" "P1Y" (showD (diffUTCDurationRollOver u1 u0))
            , nameTest "diffLocalDurationClip 2024-02-15 - 2024-01-15 = P1M" $
                let l1 = LocalTime (fromGregorian 2024 2 15) midnight
                in assertEqual "" "P1M" (showD (diffLocalDurationClip l1 l0))
            , nameTest "diffLocalDurationRollOver 2024-02-15 - 2024-01-15 = P1M" $
                let l1 = LocalTime (fromGregorian 2024 2 15) midnight
                in assertEqual "" "P1M" (showD (diffLocalDurationRollOver l1 l0))
            , nameTest "add then diff round-trips P1Y2M3D (UTC clip)" $
                let d = parseD "P1Y2M3D"
                    u1 = addUTCDurationClip d u0
                in assertEqual "" "P1Y2M3D" (showD (diffUTCDurationClip u1 u0))
            , nameTest "add then diff round-trips P1Y2M3D (Local clip)" $
                let d = parseD "P1Y2M3D"
                    l1 = addLocalDurationClip d l0
                in assertEqual "" "P1Y2M3D" (showD (diffLocalDurationClip l1 l0))
            ]

testFromCalendarDiffTime :: TestTree
testFromCalendarDiffTime =
    let
        rejectsMixed name cdt =
            nameTest name $ assertEqual "" Nothing (fromCalendarDiffTime cdt)
        accepts name cdt expected =
            nameTest name $
                case fromCalendarDiffTime cdt of
                    Just d -> assertEqual "" expected (showD d)
                    Nothing -> assertFailure "expected Just"
    in
        nameTest
            "fromCalendarDiffTime"
            [ rejectsMixed "rejects positive months / negative time"
                (CalendarDiffTime 1 (-1))
            , rejectsMixed "rejects negative months / positive time"
                (CalendarDiffTime (-1) 1)
            , accepts "zero -> P0D" (CalendarDiffTime 0 0) "P0D"
            , accepts "12 months -> P1Y" (CalendarDiffTime 12 0) "P1Y"
            , accepts "13 months -> P1Y1M" (CalendarDiffTime 13 0) "P1Y1M"
            , accepts "3600 seconds -> PT1H" (CalendarDiffTime 0 3600) "PT1H"
            , accepts "86400 seconds -> P1D" (CalendarDiffTime 0 86400) "P1D"
            , accepts "negative both -> -P1Y" (CalendarDiffTime (-12) 0) "-P1Y"
            , accepts "negative time -> -PT1H" (CalendarDiffTime 0 (-3600)) "-PT1H"
            , nameTest "round-trip via toCalendarDiffTime preserves value" $
                let d = parseD "P1Y2M3DT4H5M6.7S"
                    cdt = toCalendarDiffTime d
                in case fromCalendarDiffTime cdt of
                    Just d' -> assertEqual "" cdt (toCalendarDiffTime d')
                    Nothing -> assertFailure "expected Just"
            ]

testToCalendarDiffTimeSigns :: TestTree
testToCalendarDiffTimeSigns =
    let
        check :: String -> CalendarDiffTime -> TestTree
        check src expected =
            nameTest src $ assertEqual "" expected (toCalendarDiffTime (parseD src))
    in
        nameTest
            "toCalendarDiffTime sign handling"
            [ check "P0D" (CalendarDiffTime 0 0)
            , check "P1Y" (CalendarDiffTime 12 0)
            , check "-P1Y" (CalendarDiffTime (-12) 0)
            , check "PT1H" (CalendarDiffTime 0 3600)
            , check "-PT1H" (CalendarDiffTime 0 (-3600))
            , check "P1W" (CalendarDiffTime 0 (7 * 86400))
            , check "-P1W" (CalendarDiffTime 0 (-(7 * 86400)))
            , check "-P1Y2M3DT4H5M6S"
                (CalendarDiffTime (-(14)) (-(3 * 86400 + 4 * 3600 + 5 * 60 + 6)))
            ]

testZeroDuration :: TestTree
testZeroDuration =
    nameTest
        "zeroDuration"
        [ nameTest "shows as P0D" $
            assertEqual "" "P0D" (showD zeroDuration)
        , nameTest "is Monoid identity" $
            let d = parseD "P1Y2M3DT4H5M6S"
            in assertEqual "" (normalizeDuration d) (zeroDuration <> d)
        , nameTest "toCalendarDiffTime is mempty" $
            assertEqual "" (CalendarDiffTime 0 0) (toCalendarDiffTime zeroDuration)
        , nameTest "normalizeDuration zero == zero" $
            assertEqual "" zeroDuration (normalizeDuration zeroDuration)
        ]

testDeprecatedAliases :: TestTree
testDeprecatedAliases =
    let
        u0 = UTCTime (fromGregorian 2024 1 15) 0
        d = parseD "P1Y"
    in
        nameTest
            "deprecated addDuration{Clip,RollOver} aliases"
            [ nameTest "addDurationClip == addUTCDurationClip" $
                assertEqual "" (addUTCDurationClip d u0) (addDurationClip d u0)
            , nameTest "addDurationRollOver == addUTCDurationRollOver" $
                assertEqual "" (addUTCDurationRollOver d u0) (addDurationRollOver d u0)
            ]

testFormatTimeMore :: TestTree
testFormatTimeMore =
    let
        d = parseD "P2Y3M4DT5H6M7S"
    in
        nameTest
            "FormatTime Duration time-half specifiers"
            [ nameTest "%H" $ assertEqual "" "5" (formatTime defaultTimeLocale "%H" d)
            , nameTest "%M" $ assertEqual "" "6" (formatTime defaultTimeLocale "%M" d)
            , nameTest "%S" $ assertEqual "" "7" (formatTime defaultTimeLocale "%S" d)
            , nameTest "matches CalendarDiffTime for full string" $
                let cdt = toCalendarDiffTime d
                    fmt = "%y/%B/%H:%M:%S"
                in assertEqual ""
                    (formatTime defaultTimeLocale fmt cdt)
                    (formatTime defaultTimeLocale fmt d)
            ]

testSemigroupMore :: TestTree
testSemigroupMore =
    let
        check a b expected =
            nameTest (a ++ " <> " ++ b ++ " = " ++ expected) $
                assertEqual "" expected (showD (parseD a <> parseD b))
    in
        nameTest
            "Semigroup edge cases (XSD sec.3.3.6.1)"
            [ check "P1Y" "-P1Y" "P0D"
            , check "PT30M" "-PT15M" "PT15M"
            , check "P1Y6M" "P6M" "P2Y"
            , check "P1W" "P1W" "P14D"
            , check "P0D" "P0D" "P0D"
              -- Mixed-sign sum: months axis dominates, time
              -- absorbed.  XSD §3.3.6.1 forbids mixed-sign in the
              -- value space; the Semigroup picks the dominant axis.
            , check "P1Y" "-PT1H" "P1Y"
            , check "P2Y" "-P1M" "P1Y11M"
              -- Mixed-sign sum: time axis dominates, months absorbed.
            , check "P1M" "-P1Y" "-P11M"
            , nameTest "matches CalendarDiffTime <> on same-signed sum" $
                let a = parseD "P1Y2M3DT4H5M6S"
                    b = parseD "P3Y4M5DT6H7M8S"
                    sumD = a <> b
                    sumCDT = toCalendarDiffTime a <> toCalendarDiffTime b
                in assertEqual ""
                    sumCDT
                    (toCalendarDiffTime sumD)
            , nameTest "left identity" $
                let d = parseD "P1Y2M3DT4H5M6.78S"
                in assertEqual "" (normalizeDuration d) (mempty <> d)
            , nameTest "right identity" $
                let d = parseD "P1Y2M3DT4H5M6.78S"
                in assertEqual "" (normalizeDuration d) (d <> mempty)
            , nameTest "associativity on multi-axis sample" $
                let a = parseD "P1Y2M"
                    b = parseD "P3DT4H30M"
                    c = parseD "PT15M30.5S"
                in assertEqual "" ((a <> b) <> c) (a <> (b <> c))
            ]

testXsdCompare :: TestTree
testXsdCompare =
    let
        check name a b expected =
            nameTest name $
                assertEqual "" expected (xsdCompare (parseD a) (parseD b))
    in
        nameTest
            "xsdCompare (XSD 1.1 sec.3.3.6.1 partial order)"
            [ check "P1Y vs P1Y is EQ" "P1Y" "P1Y" (Just EQ)
            , check "P1Y vs P12M is EQ (value-equal)" "P1Y" "P12M" (Just EQ)
            , check "P2Y > P1Y" "P2Y" "P1Y" (Just GT)
            , check "P1Y < P2Y" "P1Y" "P2Y" (Just LT)
            , check "PT2H > PT1H" "PT2H" "PT1H" (Just GT)
            , check "P1M and P30D are incomparable (Nothing)"
                "P1M" "P30D" Nothing
            , check "P1M and P31D are incomparable" "P1M" "P31D" Nothing
            , check "P1Y and P365D are incomparable (leap years)"
                "P1Y" "P365D" Nothing
            , check "-P1Y < P0D" "-P1Y" "P0D" (Just LT)
            , check "P0D < P1Y" "P0D" "P1Y" (Just LT)
            , check "P1Y > P0D" "P1Y" "P0D" (Just GT)
            , check "PT1H1S > PT1H" "PT1H1S" "PT1H" (Just GT)
            ]

testOrd :: TestTree
testOrd =
    let
        ds = map parseD
            ["P0D", "P1Y", "P12M", "P30D", "P31D", "P1M", "PT1H", "PT3600S", "-P1Y", "P1Y2M3DT4H5M6S"]
        reflexive d = compare d d == EQ
        antisymmetric a b = case (compare a b, compare b a) of
            (EQ, EQ) -> True
            (LT, GT) -> True
            (GT, LT) -> True
            _ -> False
        transitive a b c = case (compare a b, compare b c) of
            (LT, LT) -> compare a c == LT
            (GT, GT) -> compare a c == GT
            (EQ, EQ) -> compare a c == EQ
            _ -> True -- not constrained
    in
        nameTest
            "Ord laws + XSD-consistency"
            [ nameTest "reflexivity on sample" $
                assertBool "" (all reflexive ds)
            , nameTest "antisymmetry on all pairs" $
                assertBool "" (and [antisymmetric a b | a <- ds, b <- ds])
            , nameTest "transitivity on all triples" $
                assertBool "" (and [transitive a b c | a <- ds, b <- ds, c <- ds])
            , nameTest "Ord agrees with xsdCompare on strict orderings" $
                -- When XSD says LT/GT, Ord must match; when XSD says
                -- EQ or Nothing (incomparable), Ord uses the
                -- structural tiebreaker — see haddock.
                let pairs = [(a, b) | a <- ds, b <- ds]
                    ok (a, b) = case xsdCompare a b of
                        Just LT -> compare a b == LT
                        Just GT -> compare a b == GT
                        _ -> True
                in assertBool "" (all ok pairs)
            , nameTest "P1M and P30D get a deterministic order despite XSD incomparable" $
                let r = compare (parseD "P1M") (parseD "P30D")
                in assertBool ("expected /= EQ; got " ++ show r) (r /= EQ)
            , nameTest "P1Y < P2Y" $
                assertEqual "" LT (compare (parseD "P1Y") (parseD "P2Y"))
            , nameTest "-P1Y < P0D < P1Y" $ do
                assertEqual "" LT (compare (parseD "-P1Y") (parseD "P0D"))
                assertEqual "" LT (compare (parseD "P0D") (parseD "P1Y"))
            , nameTest "consistency with Eq: compare a b == EQ iff a == b" $
                let pairs = [(a, b) | a <- ds, b <- ds]
                    ok (a, b) = (compare a b == EQ) == (a == b)
                in assertBool "" (all ok pairs)
            ]

testValueEqual :: TestTree
testValueEqual =
    let
        check name a b expected =
            nameTest name $
                assertEqual "" expected (valueEqual (parseD a) (parseD b))
    in
        nameTest
            "valueEqual (XSD 1.1 sec.3.3.6.1)"
            [ check "P1Y == P12M (value)" "P1Y" "P12M" True
            , check "P1W == P7D (value)" "P1W" "P7D" True
            , check "PT1H == PT3600S (value)" "PT1H" "PT3600S" True
            , check "PT1H == PT60M (value)" "PT1H" "PT60M" True
            , check "P1Y /= P1M (value)" "P1Y" "P1M" False
            , check "P1M /= P30D (value, distinct months/seconds)"
                "P1M" "P30D" False
            , nameTest "valueEqual is reflexive" $
                assertBool "" (valueEqual (parseD "P1Y2M3DT4H5M6.7S") (parseD "P1Y2M3DT4H5M6.7S"))
            , nameTest "structural Eq distinguishes P1Y and P12M" $
                assertBool "" (parseD "P1Y" /= parseD "P12M")
            ]

testNFData :: TestTree
testNFData =
    let
        forces x = (deepseq x ()) `seq` True
    in
        nameTest
            "NFData"
            [ nameTest "Duration forces" $
                assertBool "" (forces (parseD "P1Y2M3DT4H5M6S"))
            , nameTest "DurationDate (week) forces" $
                assertBool "" (forces (durDate (parseD "P1W")))
            , nameTest "zeroDuration forces" $
                assertBool "" (forces zeroDuration)
            ]

testFormatTime :: TestTree
testFormatTime =
    let
        d = parseD "P2Y3M4DT5H6M7S"
    in
        nameTest
            "FormatTime instance"
            [ nameTest "%y" $ assertEqual "" "2" (formatTime defaultTimeLocale "%y" d)
            , nameTest "%B" $ assertEqual "" "3" (formatTime defaultTimeLocale "%B" d)
            , nameTest "%b" $ assertEqual "" "27" (formatTime defaultTimeLocale "%b" d)
            ]

testXsdFormat :: TestTree
testXsdFormat =
    nameTest
        "xsdDurationFormat (XSD 1.1 sec.3.3.6.2)"
        [ nameTest "rejects PnW on read" $
            assertEqual "" Nothing (formatParseM xsdDurationFormat "P1W" :: Maybe Duration)
        , nameTest "rejects PnW on show" $
            let weekDur = parseD "P1W"
            in assertEqual "" Nothing (formatShowM xsdDurationFormat weekDur)
        , nameTest "accepts P1Y2M3DT4H5M6S" $
            assertEqual
                ""
                (Just "P1Y2M3DT4H5M6S")
                (formatShowM xsdDurationFormat =<< (formatParseM xsdDurationFormat "P1Y2M3DT4H5M6S" :: Maybe Duration))
        , nameTest "accepts leading sign (XSD prod. [15])" $
            assertEqual
                ""
                (Just "-P1Y")
                (formatShowM xsdDurationFormat =<< (formatParseM xsdDurationFormat "-P1Y" :: Maybe Duration))
        ]

testISO8601Duration :: TestTree
testISO8601Duration =
    nameTest
        "Duration"
        [ testRoundTrip
        , testRejectInvalid
        , testXSDLexicalEdges
        , testFractionalSeconds
        , testXSDCanonicalBounds
        , testXSDSignDecomposition
        , testAdditionNonCommutative
        , testXsdRejectsAlternative
        , testToCalendarDiffTime
        , testToCalendarDiffTimeSigns
        , testFromCalendarDiffTime
        , testZeroDuration
        , testNormalize
        , testSemigroup
        , testSemigroupMore
        , testScale
        , testArithmetic
        , testFormatTime
        , testFormatTimeMore
        , testXsdFormat
        , testXsdCompare
        , testOrd
        , testValueEqual
        , testDeprecatedAliases
        , testNFData
        ]
