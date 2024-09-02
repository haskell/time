{-# OPTIONS -fno-warn-orphans #-}

module Test.Format.ParseTime (
    testParseTime,
    test_parse_format,
) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.WeekDate
import Test.Arbitrary (supportedDayRange)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)
import Test.TestUtil

format :: FormatTime t => String -> t -> String
format f t = formatTime defaultTimeLocale f t

parse :: ParseTime t => Bool -> String -> String -> Maybe t
parse sp f t = parseTimeM sp defaultTimeLocale f t

data FormatOnly

data ParseAndFormat

data FormatCode pf t = MkFormatCode
    { fcModifier :: String
    , fcWidth :: Maybe Int
    , fcAlt :: Bool
    , fcSpecifier :: Char
    }

instance Show (FormatCode pf t) where
    show (MkFormatCode m w a s) =
        let
            ms = m
            ws = fromMaybe "" $ fmap show w
            as =
                if a
                    then "E"
                    else ""
            ss = [s]
        in
            '%' : (ms <> ws <> as <> ss)

formatCode :: FormatTime t => FormatCode pf t -> t -> String
formatCode fc = format $ show fc

parseCode :: ParseTime t => FormatCode ParseAndFormat t -> String -> Maybe t
parseCode fc = parse False $ show fc

class HasFormatCodes t where
    allFormatCodes :: Proxy t -> [(Bool, Char)]
    incompleteS :: Maybe t
    incompleteS = Nothing

minCodeWidth :: Char -> Int
minCodeWidth _ = 0

fcShrink :: FormatCode pf t -> [FormatCode pf t]
fcShrink fc =
    let
        fc1 = case fcWidth fc of
            Nothing -> []
            Just w
                | w > (minCodeWidth $ fcSpecifier fc) -> [fc{fcWidth = Nothing}, fc{fcWidth = Just $ w - 1}]
            Just _ -> [fc{fcWidth = Nothing}]
        fc2 = case fcAlt fc of
            False -> []
            True -> [fc{fcAlt = False}]
        fc3 = case fcModifier fc of
            "" -> []
            _ -> [fc{fcModifier = ""}]
    in
        fc1 ++ fc2 ++ fc3

instance HasFormatCodes t => Arbitrary (FormatCode FormatOnly t) where
    arbitrary = do
        m <- oneof [return "", oneof $ fmap return ["", "-", "_", "0", "^", "#"]]
        (a, s) <- oneof $ fmap return $ allFormatCodes (Proxy :: Proxy t)
        w <-
            case minCodeWidth s of
                0 -> return Nothing
                mw -> oneof [return Nothing, fmap Just $ choose (mw, 15)]
        return $ MkFormatCode m w a s
    shrink = fcShrink

instance HasFormatCodes t => Arbitrary (FormatCode ParseAndFormat t) where
    arbitrary = do
        (a, s) <- oneof $ fmap return $ allFormatCodes (Proxy :: Proxy t)
        m <-
            case s of
                'Z' -> return ""
                'z' -> return ""
                _ -> oneof [return "", oneof $ fmap return ["", "-", "_", "0"]]
        return $ MkFormatCode m Nothing a s
    shrink = fcShrink

testParseTime :: TestTree
testParseTime =
    testGroup
        "testParseTime"
        [ readsTests
        , simpleFormatTests
        , extests
        , spacingTests
        , particularParseTests
        , badParseTests
        , defaultTimeZoneTests
        , militaryTimeZoneTests
        , propertyTests
        ]

yearDays :: Integer -> [Day]
yearDays y = [(fromGregorian y 1 1) .. (fromGregorian y 12 31)]

makeExhaustiveTest :: String -> [t] -> (t -> TestTree) -> TestTree
makeExhaustiveTest name cases f = testGroup name (fmap f cases)

extests :: TestTree
extests =
    testGroup
        "exhaustive"
        ( [ makeExhaustiveTest "parse %y" [0 .. 99] parseYY
          , makeExhaustiveTest "parse %-C %y 1900s" [0, 1, 50, 99] (parseCYY 19)
          , makeExhaustiveTest "parse %-C %y 2000s" [0, 1, 50, 99] (parseCYY 20)
          , makeExhaustiveTest "parse %-C %y 1400s" [0, 1, 50, 99] (parseCYY 14)
          , makeExhaustiveTest "parse %C %y 0700s" [0, 1, 50, 99] (parseCYY2 7)
          , makeExhaustiveTest "parse %-C %y 700s" [0, 1, 50, 99] (parseCYY 7)
          , makeExhaustiveTest "parse %-C %y -700s" [0, 1, 50, 99] (parseCYY (-7))
          , makeExhaustiveTest "parse %-C %y -70000s" [0, 1, 50, 99] (parseCYY (-70000))
          , makeExhaustiveTest "parse %-C %y 10000s" [0, 1, 50, 99] (parseCYY 100)
          , makeExhaustiveTest "parse %-C centuries" [20 .. 100] (parseCentury " ")
          , makeExhaustiveTest "parse %-C century X" [1, 10, 20, 100] (parseCentury "X")
          , makeExhaustiveTest "parse %-C century 2sp" [1, 10, 20, 100] (parseCentury "  ")
          , makeExhaustiveTest "parse %-C century 5sp" [1, 10, 20, 100] (parseCentury "     ")
          ]
            ++ ( concat $
                    fmap
                        ( \y ->
                            [ {- (makeExhaustiveTest "parse %Y%m%d" (yearDays y) parseYMD)
                              , -} (makeExhaustiveTest "parse %Y %m %d" (yearDays y) parseYearDayD)
                            , (makeExhaustiveTest "parse %Y %-m %e" (yearDays y) parseYearDayE)
                            ]
                        )
                        [1, 4, 20, 753, 2000, 2011, 10001, (-1166)]
               )
        )

testReads :: (Eq a, Show a, Read a) => [(a, String)] -> String -> TestTree
testReads expected target =
    let
        found = reads target
        result = assertEqual "" expected found
        name = show target
    in
        Test.Tasty.HUnit.testCase name result

readsTestsParensSpaces ::
    forall a.
    (Eq a, Show a, Read a) =>
    a ->
    String ->
    TestTree
readsTestsParensSpaces expected target =
    testGroup
        target
        [ testReads [(expected, "")] $ target
        , testReads [(expected, "")] $ "(" ++ target ++ ")"
        , testReads [(expected, "")] $ " (" ++ target ++ ")"
        , testReads [(expected, " ")] $ " ( " ++ target ++ " ) "
        , testReads [(expected, " ")] $ " (( " ++ target ++ " )) "
        , testReads ([] :: [(a, String)]) $ "(" ++ target
        , testReads [(expected, ")")] $ "" ++ target ++ ")"
        , testReads [(expected, "")] $ "((" ++ target ++ "))"
        , testReads [(expected, " ")] $ "  (   (     " ++ target ++ "   )  ) "
        ]

readsTests :: TestTree
readsTests =
    testGroup
        "reads"
        [ readsTestsParensSpaces (3 :: Integer) "3"
        , readsTestsParensSpaces "a" "\"a\""
        , readsTestsParensSpaces testDay "1912-07-08"
        , -- , readsTestsParensSpaces testDay "1912-7-8"
          readsTestsParensSpaces testTimeOfDay "08:04:02"
          -- , readsTestsParensSpaces testTimeOfDay "8:4:2"
        ]
  where
    testDay = fromGregorian 1912 7 8
    testTimeOfDay = TimeOfDay 8 4 2

epoch :: LocalTime
epoch = LocalTime (fromGregorian 1970 0 0) midnight

testReadSTime :: (Show a, Eq a, ParseTime a) => [(a, String)] -> String -> String -> TestTree
testReadSTime expected formatStr target =
    let
        found = readSTime False defaultTimeLocale formatStr target
        result = assertEqual "" expected found
        name = (show formatStr) ++ " of " ++ (show target)
    in
        Test.Tasty.HUnit.testCase name result

simpleFormatTests :: TestTree
simpleFormatTests =
    testGroup
        "simple"
        [ testReadSTime [(epoch, "")] "" ""
        , testReadSTime [(epoch, " ")] "" " "
        , testReadSTime [(epoch, "")] " " " "
        , testReadSTime [(epoch, "")] " " "  "
        , testReadSTime [(epoch, "")] "%k" "0"
        , testReadSTime [(epoch, "")] "%k" " 0"
        , testReadSTime [(epoch, "")] "%m" "01"
        , testReadSTime [(epoch, " ")] "%m" "01 "
        , testReadSTime [(epoch, " ")] " %m" " 01 "
        , testReadSTime [(epoch, "")] " %m" " 01"
        , -- https://ghc.haskell.org/trac/ghc/ticket/9150
          testReadSTime [(epoch, "")] " %M" " 00"
        , testReadSTime [(epoch, "")] "%M " "00 "
        , testReadSTime [(epoch, "")] "%Q" ""
        , testReadSTime [(epoch, " ")] "%Q" " "
        , testReadSTime [(epoch, "X")] "%Q" "X"
        , testReadSTime [(epoch, " X")] "%Q" " X"
        , testReadSTime [(epoch, "")] "%Q " " "
        , testReadSTime [(epoch, "")] "%Q X" " X"
        , testReadSTime [(epoch, "")] "%QX" "X"
        ]

spacingForFormatTests :: (Show t, Eq t, ParseTime t) => t -> String -> String -> TestTree
spacingForFormatTests expected formatStr target =
    testGroup
        formatStr
        [ parseTest False (Just expected) formatStr target
        , parseTest True (Just expected) formatStr target
        , parseTest False (Just expected) (formatStr ++ " ") (target ++ " ")
        , parseTest True (Just expected) (formatStr ++ " ") (target ++ " ")
        , parseTest False (Just expected) (" " ++ formatStr) (" " ++ target)
        , parseTest True (Just expected) (" " ++ formatStr) (" " ++ target)
        , parseTest True (Just expected) ("" ++ formatStr) (" " ++ target)
        , parseTest True (Just expected) (" " ++ formatStr) ("  " ++ target)
        ]

spacingTests :: TestTree
spacingTests =
    testGroup
        "spacing"
        [ spacingForFormatTests epoch "%Q" ""
        , spacingForFormatTests epoch "%Q" ".0"
        , spacingForFormatTests epoch "%k" " 0"
        , spacingForFormatTests epoch "%M" "00"
        , spacingForFormatTests epoch "%m" "01"
        , spacingForFormatTests (TimeZone 120 False "") "%z" "+0200"
        , spacingForFormatTests (TimeZone 120 False "") "%Z" "+0200"
        , spacingForFormatTests (TimeZone (-480) False "PST") "%Z" "PST"
        ]

particularParseTests :: TestTree
particularParseTests =
    testGroup
        "particular"
        [ parseTest @Day True Nothing "%-d%-m%0Y" "2122012" -- ISSUE #232
        , parseTest @Day True Nothing "%-d%-m%0Y" "2132012" -- ISSUE #232
        ]

badParseTests :: TestTree
badParseTests = testGroup "bad" [parseTest False (Nothing :: Maybe Day) "%Y" ""]

{-
parseYMD :: Day -> TestTree
parseYMD day = case toGregorian day of
    (y, m, d) -> parseTest False (Just day) "%Y%m%d" ((show y) ++ (show2 m) ++ (show2 d))
-}

parseYearDayD :: Day -> TestTree
parseYearDayD day = case toGregorian day of
    (y, m, d) -> parseTest False (Just day) "%Y %m %d" ((show y) ++ " " ++ (show2 m) ++ " " ++ (show2 d))

parseYearDayE :: Day -> TestTree
parseYearDayE day = case toGregorian day of
    (y, m, d) -> parseTest False (Just day) "%Y %-m %e" ((show y) ++ " " ++ (show m) ++ " " ++ (show d))

-- | 1969 - 2068
expectedYear :: Integer -> Integer
expectedYear i | i >= 69 = 1900 + i
expectedYear i = 2000 + i

show2 :: (Show n, Integral n) => n -> String
show2 i = (show (div i 10)) ++ (show (mod i 10))

parseYY :: Integer -> TestTree
parseYY i = parseTest False (Just (fromGregorian (expectedYear i) 1 1)) "%y" (show2 i)

parseCYY :: Integer -> Integer -> TestTree
parseCYY c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%-C %y" ((show c) ++ " " ++ (show2 i))

parseCYY2 :: Integer -> Integer -> TestTree
parseCYY2 c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%C %y" ((show2 c) ++ " " ++ (show2 i))

parseCentury :: String -> Integer -> TestTree
parseCentury int c =
    parseTest False (Just (fromGregorian (c * 100) 1 1)) ("%-C" ++ int ++ "%y") ((show c) ++ int ++ "00")

parseTest :: forall t. (Show t, Eq t, ParseTime t) => Bool -> Maybe t -> String -> String -> TestTree
parseTest sp expected formatStr target =
    let
        found = parse sp formatStr target
        result = assertEqual "" expected found
        name =
            (show formatStr)
                ++ " of "
                ++ (show target)
                ++ ( if sp
                        then " allowing spaces"
                        else ""
                   )
    in
        Test.Tasty.HUnit.testCase name result

enumAdd :: Enum a => Int -> a -> a
enumAdd i a = toEnum (i + fromEnum a)

getMilZoneLetter :: Int -> Char
getMilZoneLetter 0 = 'Z'
getMilZoneLetter h | h < 0 = enumAdd (negate h) 'M'
getMilZoneLetter h | h < 10 = enumAdd (h - 1) 'A'
getMilZoneLetter h = enumAdd (h - 10) 'K'

getMilZone :: Int -> TimeZone
getMilZone hour = TimeZone (hour * 60) False [getMilZoneLetter hour]

testParseTimeZone :: TimeZone -> TestTree
testParseTimeZone tz = parseTest False (Just tz) "%Z" (timeZoneName tz)

defaultTimeZoneTests :: TestTree
defaultTimeZoneTests = testGroup "default time zones" (fmap testParseTimeZone (knownTimeZones defaultTimeLocale))

militaryTimeZoneTests :: TestTree
militaryTimeZoneTests = testGroup "military time zones" (fmap (testParseTimeZone . getMilZone) [-12 .. 12])

-- missing from the time package
instance Eq ZonedTime where
    ZonedTime t1 tz1 == ZonedTime t2 tz2 = t1 == t2 && tz1 == tz2

compareResult' :: (Eq a, Show a) => String -> a -> a -> Result
compareResult' extra expected found
    | expected == found = succeeded
    | otherwise = failed{reason = "expected " ++ (show expected) ++ ", found " ++ (show found) ++ extra}

compareResult :: (Eq a, Show a) => a -> a -> Result
compareResult = compareResult' ""

compareParse ::
    forall a.
    (Eq a, Show a, ParseTime a) =>
    a ->
    String ->
    String ->
    Result
compareParse expected fmt text = compareResult' (", parsing " ++ (show text)) (Just expected) (parse False fmt text)

--

-- * tests for debugging failing cases

--
test_parse_format :: (FormatTime t, ParseTime t, Show t) => String -> t -> (String, String, Maybe t)
test_parse_format f t =
    let
        s = format f t
    in
        (show t, s, parse False f s `asTypeOf` Just t)

--

-- * show and read

--
reads_expect :: a -> [(a, String)]
reads_expect t = [(t, "")]

prop_read_show :: (Read a, Show a, Eq a) => a -> Result
prop_read_show t = compareResult (reads_expect t) (reads (show t))

prop_read_show_ZonedUTC :: ZonedTime -> Result
prop_read_show_ZonedUTC t = compareResult (reads_expect $ zonedTimeToUTC t) (reads (show t))

prop_read_show_LocalUTC :: LocalTime -> Result
prop_read_show_LocalUTC t = compareResult (reads_expect $ localTimeToUTC utc t) (reads (show t))

prop_read_show_UTC_no_TZ :: UTCTime -> Result
prop_read_show_UTC_no_TZ t = compareResult (reads_expect t) $ reads $ show $ utcToLocalTime utc t

--

-- * special show functions

--
prop_parse_showWeekDate :: Day -> Result
prop_parse_showWeekDate d = compareParse d "%G-W%V-%u" (showWeekDate d)

prop_parse_showGregorian :: Day -> Result
prop_parse_showGregorian d = compareParse d "%Y-%m-%d" (showGregorian d)

prop_parse_showOrdinalDate :: Day -> Result
prop_parse_showOrdinalDate d = compareParse d "%Y-%j" (showOrdinalDate d)

--

-- * fromMondayStartWeek and fromSundayStartWeek

--
prop_fromMondayStartWeek :: Day -> Result
prop_fromMondayStartWeek d =
    let
        (w, wd) = mondayStartWeek d
        (y, _, _) = toGregorian d
    in
        compareResult d (fromMondayStartWeek y w wd)

prop_fromSundayStartWeek :: Day -> Result
prop_fromSundayStartWeek d =
    let
        (w, wd) = sundayStartWeek d
        (y, _, _) = toGregorian d
    in
        compareResult d (fromSundayStartWeek y w wd)

-- t == parse (format t)
prop_parse_format :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format (FormatString f) t = compareParse t f (format f t)

-- t == parse (upper (format t))
prop_parse_format_upper :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_upper (FormatString f) t = compareParse t f (map toUpper $ format f t)

-- t == parse (lower (format t))
prop_parse_format_lower :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_lower (FormatString f) t = compareParse t f (map toLower $ format f t)

-- Default time is 1970-01-01 00:00:00 +0000 (which was a Thursday)
in1970 :: Maybe String -> Char -> String -> Maybe String
in1970 _ 'j' "366" = Nothing -- 1970 was not a leap year
in1970 _ 'U' "53" = Nothing -- last day of 1970 was Sunday-start-week 52
in1970 _ 'W' "53" = Nothing -- last day of 1970 was Monday-start-week 52
in1970 (Just s) 'S' "60" = Just s -- no leap second without other data
in1970 _ _ s = Just s

-- format t == format (parse (format t))
prop_format_parse_format ::
    forall t.
    (HasFormatCodes t, FormatTime t, ParseTime t) =>
    Proxy t ->
    FormatCode ParseAndFormat t ->
    t ->
    Result
prop_format_parse_format _ fc v =
    let
        s1 = formatCode fc v
        ms1 = in1970 (fmap (formatCode fc) (incompleteS :: Maybe t)) (fcSpecifier fc) s1
        mv2 :: Maybe t
        mv2 = parseCode fc s1
        ms2 = fmap (formatCode fc) mv2
    in
        compareResult ms1 ms2

instance HasFormatCodes Day where
    allFormatCodes _ = [(False, s) | s <- "DFxYyCBbhmdejfVUW"]

instance HasFormatCodes TimeOfDay where
    allFormatCodes _ = [(False, s) | s <- "RTXrPpHkIlMSqQ"]

instance HasFormatCodes DayOfWeek where
    allFormatCodes _ = [(False, s) | s <- "uwaA"]

instance HasFormatCodes Month where
    allFormatCodes _ = [(False, s) | s <- "YyCBbhm"]

instance HasFormatCodes LocalTime where
    allFormatCodes _ = allFormatCodes (Proxy :: Proxy Day) ++ allFormatCodes (Proxy :: Proxy TimeOfDay)

instance HasFormatCodes TimeZone where
    allFormatCodes _ = [(a, s) | a <- [False, True], s <- "zZ"]

instance HasFormatCodes ZonedTime where
    allFormatCodes _ =
        [(False, s) | s <- "cs"]
            ++ allFormatCodes (Proxy :: Proxy LocalTime)
            ++ allFormatCodes (Proxy :: Proxy TimeZone)

instance HasFormatCodes UTCTime where
    allFormatCodes _ = [(False, s) | s <- "cs"] ++ allFormatCodes (Proxy :: Proxy LocalTime)
    incompleteS = Just $ UTCTime (fromGregorian 2000 1 1) 0

instance HasFormatCodes UniversalTime where
    allFormatCodes _ = allFormatCodes (Proxy :: Proxy LocalTime)

--

-- * crashes in parse

--
newtype Input
    = Input String

instance Show Input where
    show (Input s) = s

instance Arbitrary Input where
    arbitrary = liftM Input $ list cs
      where
        cs = elements (['0' .. '9'] ++ ['-', ' ', '/'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'])
        list g = sized (\n -> choose (0, n) >>= \l -> replicateM l g)

instance CoArbitrary Input where
    coarbitrary (Input s) = coarbitrary (sum (map ord s))

prop_no_crash_bad_input :: (Eq t, ParseTime t) => FormatString t -> Input -> Property
prop_no_crash_bad_input fs@(FormatString f) (Input s) =
    property $
        case parse False f s of
            Nothing -> True
            Just t -> t == t `asTypeOf` formatType fs

--
--
--
newtype FormatString a
    = FormatString String

formatType :: FormatString t -> t
formatType _ = undefined

instance Show (FormatString a) where
    show (FormatString f) = show f

typedTests :: (forall t. (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result) -> [TestTree]
typedTests prop =
    [ nameTest "Day" $ tgroup dayFormats prop
    , nameTest "Month" $ tgroup monthFormats prop
    , nameTest "TimeOfDay" $ tgroup timeOfDayFormats prop
    , nameTest "LocalTime" $ tgroup localTimeFormats prop
    , nameTest "TimeZone" $ tgroup timeZoneFormats prop
    , nameTest "ZonedTime" $ tgroup zonedTimeFormats prop
    , nameTest "ZonedTime" $
        tgroup zonedTimeAlmostFormats $
            \fmt t -> (todSec $ localTimeOfDay $ zonedTimeToLocalTime t) < 60 ==> prop fmt t
    , nameTest "UTCTime" $ tgroup utcTimeAlmostFormats $ \fmt t -> utctDayTime t < 86400 ==> prop fmt t
    , nameTest "UniversalTime" $ tgroup universalTimeFormats prop
    , nameTest "CalendarDiffDays" $ tgroup calendarDiffDaysFormats prop
    , nameTest "CalenderDiffTime" $ tgroup calendarDiffTimeFormats prop
    , nameTest "DiffTime" $ tgroup diffTimeFormats prop
    , nameTest "NominalDiffTime" $ tgroup nominalDiffTimeFormats prop
    ]

allTypes ::
    (forall t. (Eq t, Show t, Arbitrary t, FormatTime t, ParseTime t, HasFormatCodes t) => String -> Proxy t -> r) ->
    [r]
allTypes f =
    [ f "Day" (Proxy :: Proxy Day)
    , f "TimeOfDay" (Proxy :: Proxy TimeOfDay)
    , f "DayOfWeek" (Proxy :: Proxy DayOfWeek)
    , f "Month" (Proxy :: Proxy Month)
    , f "LocalTime" (Proxy :: Proxy LocalTime)
    , f "TimeZone" (Proxy :: Proxy TimeZone)
    , f "ZonedTime" (Proxy :: Proxy ZonedTime)
    , f "UTCTime" (Proxy :: Proxy UTCTime)
    , f "UniversalTime" (Proxy :: Proxy UniversalTime)
    ]

allLeapSecondTypes ::
    (forall t. (Eq t, Show t, Arbitrary t, FormatTime t, ParseTime t, HasFormatCodes t) => String -> t -> r) ->
    [r]
allLeapSecondTypes f =
    let
        day :: Day
        day = fromGregorian 2000 01 01
        lsTimeOfDay :: TimeOfDay
        lsTimeOfDay = TimeOfDay 23 59 60.5
        lsLocalTime :: LocalTime
        lsLocalTime = LocalTime day lsTimeOfDay
        lsZonedTime :: ZonedTime
        lsZonedTime = ZonedTime lsLocalTime utc
        lsUTCTime :: UTCTime
        lsUTCTime = UTCTime day 86400.5
    in
        [ f "TimeOfDay" lsTimeOfDay
        , f "LocalTime" lsLocalTime
        , f "ZonedTime" lsZonedTime
        , f "UTCTime" lsUTCTime
        ]

parseEmptyTest ::
    forall t.
    ParseTime t =>
    Proxy t ->
    Assertion
parseEmptyTest _ =
    case parse False "" "" :: Maybe t of
        Just _ -> return ()
        Nothing -> assertFailure "failed"

parseEmptyTests :: TestTree
parseEmptyTests = nameTest "parse empty" $ allTypes $ \name p -> nameTest name $ parseEmptyTest p

prop_parse_s :: forall a. (Eq a, Show a, ParseTime a) => (UTCTime -> a) -> UTCTime -> Result
prop_parse_s f t =
    let
        str = format "%s%Q" t
        found = parse False "%s%Q" str
        expected = f t
    in
        compareResult (Just expected) found

prop_parse_sz :: forall a. (Eq a, Show a, ParseTime a) => (ZonedTime -> a) -> ZonedTime -> Result
prop_parse_sz f t =
    let
        str = format "%s%Q %z" t
        found = parse False "%s%Q %z" str
        expected = f t
    in
        compareResult (Just expected) found

zeroTimeZone :: TimeZone
zeroTimeZone = TimeZone 0 False ""

parse_s_tests :: TestTree
parse_s_tests =
    nameTest
        "parse_s"
        [ nameTest "UTCTime" $ prop_parse_s @UTCTime id
        , nameTest "ZonedTime" $ prop_parse_s @ZonedTime $ utcToZonedTime zeroTimeZone
        , nameTest "TimeZone" $ prop_parse_s @TimeZone $ \_ -> zeroTimeZone
        , nameTest "LocalTime" $ prop_parse_s @LocalTime $ utcToLocalTime zeroTimeZone
        , nameTest "Day" $ prop_parse_s @Day utctDay
        , nameTest "Month" $ prop_parse_s @Month $ (\(MonthDay m _) -> m) . utctDay
        , nameTest "DayOfWeek" $ prop_parse_s @DayOfWeek $ dayOfWeek . utctDay
        , nameTest "TimeOfDay" $ prop_parse_s @TimeOfDay $ localTimeOfDay . utcToLocalTime zeroTimeZone
        ]

parse_sz_tests :: TestTree
parse_sz_tests =
    nameTest
        "parse_sz"
        [ nameTest "UTCTime" $ prop_parse_sz @UTCTime zonedTimeToUTC
        , nameTest "ZonedTime" $ prop_parse_sz @ZonedTime id
        , nameTest "TimeZone" $ prop_parse_sz @TimeZone zonedTimeZone
        , nameTest "LocalTime" $ prop_parse_sz @LocalTime zonedTimeToLocalTime
        , nameTest "Day" $ prop_parse_sz @Day $ localDay . zonedTimeToLocalTime
        , nameTest "Month" $ prop_parse_sz @Month $ (\(MonthDay m _) -> m) . localDay . zonedTimeToLocalTime
        , nameTest "DayOfWeek" $ prop_parse_sz @DayOfWeek $ dayOfWeek . localDay . zonedTimeToLocalTime
        , nameTest "TimeOfDay" $ prop_parse_sz @TimeOfDay $ localTimeOfDay . zonedTimeToLocalTime
        ]

formatParseFormatTests :: TestTree
formatParseFormatTests =
    nameTest
        "format_parse_format"
        [ localOption (QuickCheckTests 50000) $
            nameTest "general" $
                allTypes $
                    \name p -> nameTest name $ prop_format_parse_format p
        , nameTest "#177" $
            [ nameTest "start" $ \fc -> prop_format_parse_format Proxy fc (fst supportedDayRange)
            , nameTest "end" $ \fc -> prop_format_parse_format Proxy fc (snd supportedDayRange)
            ]
        , nameTest "leapsecond" $ allLeapSecondTypes $ \name t -> nameTest name $ \fc -> prop_format_parse_format Proxy fc t
        ]

badInputTests :: TestTree
badInputTests =
    nameTest
        "no_crash_bad_input"
        [ nameTest "Day" $ tgroup (dayFormats ++ partialDayFormats ++ failingPartialDayFormats) prop_no_crash_bad_input
        , nameTest "TimeOfDay" $ tgroup (timeOfDayFormats ++ partialTimeOfDayFormats) prop_no_crash_bad_input
        , nameTest "LocalTime" $ tgroup (localTimeFormats ++ partialLocalTimeFormats) prop_no_crash_bad_input
        , nameTest "TimeZone" $ tgroup (timeZoneFormats) prop_no_crash_bad_input
        , nameTest "ZonedTime" $
            tgroup (zonedTimeFormats ++ zonedTimeAlmostFormats ++ partialZonedTimeFormats) prop_no_crash_bad_input
        , nameTest "UTCTime" $ tgroup (utcTimeAlmostFormats ++ partialUTCTimeFormats) prop_no_crash_bad_input
        , nameTest "UniversalTime" $
            tgroup (universalTimeFormats ++ partialUniversalTimeFormats) prop_no_crash_bad_input
        ]

readShowTests :: TestTree
readShowTests =
    nameTest
        "read_show"
        [ nameTest "Day" (prop_read_show :: Day -> Result)
        , nameTest "Month" (prop_read_show :: Month -> Result)
        , nameTest "QuarterOfYear" (prop_read_show :: QuarterOfYear -> Result)
        , nameTest "Quarter" (prop_read_show :: Quarter -> Result)
        , nameTest "TimeOfDay" (prop_read_show :: TimeOfDay -> Result)
        , nameTest "LocalTime" (prop_read_show :: LocalTime -> Result)
        , nameTest "TimeZone" (prop_read_show :: TimeZone -> Result)
        , nameTest "ZonedTime" (prop_read_show :: ZonedTime -> Result)
        , nameTest "UTCTime" (prop_read_show :: UTCTime -> Result)
        , nameTest "UTCTime (zoned)" prop_read_show_ZonedUTC
        , nameTest "UTCTime (local)" prop_read_show_LocalUTC
        , nameTest "UTCTime (no TZ)" prop_read_show_UTC_no_TZ
        , nameTest "UniversalTime" (prop_read_show :: UniversalTime -> Result)
        , nameTest "NominalDiffTime" (prop_read_show :: NominalDiffTime -> Result)
        , nameTest "DiffTime" (prop_read_show :: DiffTime -> Result)
        , nameTest "CalendarDiffDays" (prop_read_show :: CalendarDiffDays -> Result)
        , nameTest "CalendarDiffTime" (prop_read_show :: CalendarDiffTime -> Result)
        ]

parseShowTests :: TestTree
parseShowTests =
    nameTest
        "parse_show"
        [ nameTest "showWeekDate" prop_parse_showWeekDate
        , nameTest "showGregorian" prop_parse_showGregorian
        , nameTest "showOrdinalDate" prop_parse_showOrdinalDate
        ]

propertyTests :: TestTree
propertyTests =
    localOption (QuickCheckTests 2000) $
        nameTest
            "properties"
            [ readShowTests
            , parseShowTests
            , nameTest "fromMondayStartWeek" prop_fromMondayStartWeek
            , nameTest "fromSundayStartWeek" prop_fromSundayStartWeek
            , nameTest "parse_format" $ typedTests prop_parse_format
            , nameTest "parse_format_lower" $ typedTests prop_parse_format_lower
            , nameTest "parse_format_upper" $ typedTests prop_parse_format_upper
            , parseEmptyTests
            , parse_s_tests
            , parse_sz_tests
            , formatParseFormatTests
            , badInputTests
            ]

dayFormats :: [FormatString Day]
dayFormats =
    map
        FormatString
        -- numeric year, month, day
        [ "%Y-%m-%d"
        , "%Y%m%d"
        , "%C%y%m%d"
        , "%Y %m %e"
        , "%m/%d/%Y"
        , "%d/%m/%Y"
        , "%Y/%d/%m"
        , "%D %C"
        , "%F"
        , -- month names
          "%Y-%B-%d"
        , "%Y-%b-%d"
        , "%Y-%h-%d"
        , "%C-%y-%B-%d"
        , "%C-%y-%b-%d"
        , "%C-%y-%h-%d"
        , -- ordinal dates
          "%Y-%j"
        , "%C-%y-%j"
        , -- ISO week dates
          "%G-%V-%u"
        , "%G-%V-%a"
        , "%G-%V-%A"
        , "%G-%V-%w"
        , "%A week %V, %G"
        , "day %V, week %A, %G"
        , "%G-W%V-%u"
        , "%f%g-%V-%u"
        , "%f%g-%V-%a"
        , "%f%g-%V-%A"
        , "%f%g-%V-%w"
        , "%A week %V, %f%g"
        , "day %V, week %A, %f%g"
        , "%f%g-W%V-%u"
        , -- monday and sunday week dates
          "%Y-w%U-%A"
        , "%Y-w%W-%A"
        , "%Y-%A-w%U"
        , "%Y-%A-w%W"
        , "%A week %U, %Y"
        , "%A week %W, %Y"
        ]

monthFormats :: [FormatString Month]
monthFormats =
    map
        FormatString
        -- numeric year, month
        [ "%Y-%m"
        , "%Y%m"
        , "%C%y%m"
        , "%Y %m"
        , "%m/%Y"
        , "%m/%Y"
        , "%Y/%m"
        , "%C %y %m"
        , -- month names
          "%Y-%B"
        , "%Y-%b"
        , "%Y-%h"
        , "%C-%y-%B"
        , "%C-%y-%b"
        , "%C-%y-%h"
        ]

timeOfDayFormats :: [FormatString TimeOfDay]
timeOfDayFormats =
    map
        FormatString
        -- 24 h formats
        [ "%H:%M:%S.%q"
        , "%k:%M:%S.%q"
        , "%H%M%S.%q"
        , "%T.%q"
        , "%X.%q"
        , "%R:%S.%q"
        , "%H:%M:%S%Q"
        , "%k:%M:%S%Q"
        , "%H%M%S%Q"
        , "%T%Q"
        , "%X%Q"
        , "%R:%S%Q"
        , -- 12 h formats
          "%I:%M:%S.%q %p"
        , "%I:%M:%S.%q %P"
        , "%l:%M:%S.%q %p"
        , "%r %q"
        , "%I:%M:%S%Q %p"
        , "%I:%M:%S%Q %P"
        , "%l:%M:%S%Q %p"
        , "%r %Q"
        ]

localTimeFormats :: [FormatString LocalTime]
localTimeFormats = map FormatString [] {-"%Q","%Q ","%QX"-}

timeZoneFormats :: [FormatString TimeZone]
timeZoneFormats = map FormatString ["%z", "%z%Z", "%Z%z", "%Z", "%Ez", "%EZ"]

zonedTimeFormats :: [FormatString ZonedTime]
zonedTimeFormats =
    map
        FormatString
        [ "%a, %d %b %Y %H:%M:%S.%q %z"
        , "%a, %d %b %Y %H:%M:%S%Q %z"
        , "%a, %d %b %Y %H:%M:%S.%q %Z"
        , "%a, %d %b %Y %H:%M:%S%Q %Z"
        ]

zonedTimeAlmostFormats :: [FormatString ZonedTime]
zonedTimeAlmostFormats = map FormatString ["%s.%q %z", "%s%Q %z", "%s.%q %Z", "%s%Q %Z"]

utcTimeAlmostFormats :: [FormatString UTCTime]
utcTimeAlmostFormats = map FormatString ["%s.%q", "%s%Q"]

universalTimeFormats :: [FormatString UniversalTime]
universalTimeFormats = map FormatString []

calendarDiffDaysFormats :: [FormatString CalendarDiffDays]
calendarDiffDaysFormats = map FormatString ["%yy%Bm%ww%Dd", "%yy%Bm%dd", "%bm%ww%Dd", "%bm%dd"]

calendarDiffTimeFormats :: [FormatString CalendarDiffTime]
calendarDiffTimeFormats =
    map
        FormatString
        [ "%yy%Bm%ww%Dd%Hh%Mm%ESs"
        , "%bm%ww%Dd%Hh%Mm%ESs"
        , "%bm%dd%Hh%Mm%ESs"
        , "%bm%hh%Mm%ESs"
        , "%bm%mm%ESs"
        , "%bm%mm%0ESs"
        , "%bm%Ess"
        , "%bm%0Ess"
        ]

diffTimeFormats :: [FormatString DiffTime]
diffTimeFormats =
    map FormatString ["%ww%Dd%Hh%Mm%ESs", "%dd%Hh%Mm%ESs", "%hh%Mm%ESs", "%mm%ESs", "%mm%0ESs", "%Ess", "%0Ess"]

nominalDiffTimeFormats :: [FormatString NominalDiffTime]
nominalDiffTimeFormats =
    map FormatString ["%ww%Dd%Hh%Mm%ESs", "%dd%Hh%Mm%ESs", "%hh%Mm%ESs", "%mm%ESs", "%mm%0ESs", "%Ess", "%0Ess"]

--

-- * Formats that do not include all the information

--
partialDayFormats :: [FormatString Day]
partialDayFormats = map FormatString []

partialTimeOfDayFormats :: [FormatString TimeOfDay]
partialTimeOfDayFormats = map FormatString ["%H", "%M", "%S", "%H:%M"]

partialLocalTimeFormats :: [FormatString LocalTime]
partialLocalTimeFormats = map FormatString []

partialZonedTimeFormats :: [FormatString ZonedTime]
partialZonedTimeFormats =
    map
        FormatString
        -- %s does not include second decimals
        [ "%s %z"
        , -- %S does not include second decimals
          "%c"
        , "%a, %d %b %Y %H:%M:%S %Z"
        ]

partialUTCTimeFormats :: [FormatString UTCTime]
partialUTCTimeFormats =
    map
        FormatString
        -- %s does not include second decimals
        [ "%s"
        , -- %c does not include second decimals
          "%c"
        ]

partialUniversalTimeFormats :: [FormatString UniversalTime]
partialUniversalTimeFormats = map FormatString []

failingPartialDayFormats :: [FormatString Day]
failingPartialDayFormats =
    map
        FormatString
        -- ISO week dates with two digit year.
        -- This can fail in the beginning or the end of a year where
        -- the ISO week date year does not match the gregorian year.
        ["%g-%V-%u", "%g-%V-%a", "%g-%V-%A", "%g-%V-%w", "%A week %V, %g", "day %V, week %A, %g", "%g-W%V-%u"]
