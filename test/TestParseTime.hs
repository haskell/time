{-# OPTIONS -Wall -Werror -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

import Control.Monad
import Data.Char
import Data.Ratio
import Data.Maybe
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.POSIX
import System.Locale
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Batch


class RunTest p where
    runTest :: p -> IO TestResult

instance RunTest (IO TestResult) where
    runTest iob = iob

instance RunTest Property where
    runTest p = run p (TestOptions {no_of_tests = 10000,length_of_tests = 0,debug_tests = False})

data ExhaustiveTest = forall t. (Show t) => MkExhaustiveTest [t] (t -> IO Bool)

instance RunTest ExhaustiveTest where
    runTest (MkExhaustiveTest cases f) = do
        results <- mapM (\t -> do {b <- f t;return (b,show t)}) cases
        let failures = mapMaybe (\(b,n) -> if b then Nothing else Just n) results
        let fcount = length failures
        return (if fcount == 0 then TestOk "OK" 0 [] else TestFailed failures fcount)

ntest :: Int
ntest = 1000

main :: IO ()
main = do
    putStrLn "Should work:"
    good1 <- checkAll extests
    putStrLn "Should work:"
    good2 <- checkAll properties
    putStrLn "Known failures:"
    _ <- checkAll knownFailures
    exitWith (if good1 && good2 then ExitSuccess else ExitFailure 1)

days2011 :: [Day]
days2011 = [(fromGregorian 2011 1 1) .. (fromGregorian 2011 12 31)]

extests :: [(String,ExhaustiveTest)]
extests = [
    ("parse %y",MkExhaustiveTest [0..99] parseYY),
    ("parse %C %y 1900s",MkExhaustiveTest [0..99] (parseCYY 19)),
    ("parse %C %y 2000s",MkExhaustiveTest [0..99] (parseCYY 20)),
    ("parse %C %y 1400s",MkExhaustiveTest [0..99] (parseCYY 14)),
    ("parse %C %y 700s",MkExhaustiveTest [0..99] (parseCYY 7)),
    ("parseYearDay %Y %m %d",MkExhaustiveTest days2011 parseYearDayD),
    ("parseYearDay %Y %m %d 0-pad",MkExhaustiveTest days2011 parseYearDayD2),
    ("parseYearDay %Y %m %e",MkExhaustiveTest days2011 parseYearDayE),
    ("parseYearDay %Y %m %e 0-pad",MkExhaustiveTest days2011 parseYearDayE2)
    ]

parseYearDayD :: Day -> IO Bool
parseYearDayD day = case toGregorian day of
    (y,m,d) -> return $ (parse "%Y %m %d" ((show y) ++ " " ++ (show m) ++ " " ++ (show d))) == Just day

parseYearDayD2 :: Day -> IO Bool
parseYearDayD2 day = case toGregorian day of
    (y,m,d) -> return $ (parse "%Y %m %d" ((show y) ++ " " ++ (show2 m) ++ " " ++ (show2 d))) == Just day

parseYearDayE :: Day -> IO Bool
parseYearDayE day = case toGregorian day of
    (y,m,d) -> return $ (parse "%Y %m %e" ((show y) ++ " " ++ (show m) ++ " " ++ (show d))) == Just day

parseYearDayE2 :: Day -> IO Bool
parseYearDayE2 day = case toGregorian day of
    (y,m,d) -> return $ (parse "%Y %m %e" ((show y) ++ " " ++ (show2 m) ++ " " ++ (show2 d))) == Just day

-- | 1969 - 2068
expectedYear :: Integer -> Integer
expectedYear i | i >= 69 = 1900 + i
expectedYear i = 2000 + i

show2 :: (Integral n) => n -> String
show2 i = (show (div i 10)) ++ (show (mod i 10))

parseYY :: Integer -> IO Bool
parseYY i = return (parse "%y" (show2 i) == Just (fromGregorian (expectedYear i) 1 1))

parseCYY :: Integer -> Integer -> IO Bool
parseCYY c i = return (parse "%C %y" ((show2 c) ++ " " ++ (show2 i)) == Just (fromGregorian ((c * 100) + i) 1 1))

checkAll :: RunTest p => [(String,p)] -> IO Bool
checkAll ps = fmap and (mapM checkOne ps)

trMessage :: TestResult -> String
trMessage (TestOk s _ _) = s
trMessage (TestExausted s i ss) = "Exhausted " ++ (show s) ++ " " ++ (show i) ++ " " ++ (show ss)
trMessage (TestFailed ss i) = "Failed " ++ (show ss) ++ " " ++ (show i)
trMessage (TestAborted ex) = "Aborted " ++ (show ex)

trGood :: TestResult -> Bool
trGood (TestOk _ _ _) = True
trGood _ = False

checkOne :: RunTest p => (String,p) -> IO Bool
checkOne (n,p) =
    do
       putStr (rpad 65 ' ' n)
       tr <- runTest p
       putStrLn (trMessage tr)
       return (trGood tr)
  where
    rpad n' c xs = xs ++ replicate (n' - length xs) c


parse :: ParseTime t => String -> String -> Maybe t
parse f t = parseTime defaultTimeLocale f t

format :: (FormatTime t) => String -> t -> String
format f t = formatTime defaultTimeLocale f t


instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
        where intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
              fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10^12)
              secondsToDiffTime' :: Integer -> DiffTime
              secondsToDiffTime' = fromInteger
              picosecondsToDiffTime' :: Integer -> DiffTime
              picosecondsToDiffTime' x = fromRational (x % 10^12)
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary TimeOfDay where
    arbitrary = liftM timeToTimeOfDay arbitrary
    coarbitrary t = coarbitrary (timeOfDayToTime t)

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds (localTimeToUTC utc t)) :: Integer)

instance Arbitrary TimeZone where
    arbitrary = liftM minutesToTimeZone $ choose (-720,720)
    coarbitrary tz = coarbitrary (timeZoneMinutes tz)

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds (zonedTimeToUTC t)) :: Integer)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds t) :: Integer)

-- missing from the time package
instance Eq ZonedTime where
    ZonedTime t1 tz1 == ZonedTime t2 tz2 = t1 == t2 && tz1 == tz2

-- 
-- * tests for dbugging failing cases
--

test_parse_format :: (FormatTime t,ParseTime t,Show t) => String -> t -> (String,String,Maybe t)
test_parse_format f t = let s = format f t in (show t, s, parse f s `asTypeOf` Just t)

--
-- * show and read 
--

prop_read_show :: (Read a, Show a, Eq a) => a -> Bool
prop_read_show t = read (show t) == t

--
-- * special show functions
--

prop_parse_showWeekDate :: Day -> Bool
prop_parse_showWeekDate d = parse "%G-W%V-%u" (showWeekDate d) == Just d

prop_parse_showGregorian :: Day -> Bool
prop_parse_showGregorian d = parse "%Y-%m-%d" (showGregorian d) == Just d

prop_parse_showOrdinalDate :: Day -> Bool
prop_parse_showOrdinalDate d = parse "%Y-%j" (showOrdinalDate d) == Just d

--
-- * fromMondayStartWeek and fromSundayStartWeek
--

prop_fromMondayStartWeek :: Day -> Bool
prop_fromMondayStartWeek d = 
    let (w,wd)  = mondayStartWeek d
        (y,_,_) = toGregorian d
     in fromMondayStartWeek y w wd == d

prop_fromSundayStartWeek :: Day -> Bool
prop_fromSundayStartWeek d = 
    let (w,wd)  = sundayStartWeek d
        (y,_,_) = toGregorian d
     in fromSundayStartWeek y w wd == d

--
-- * format and parse 
--

-- | Helper for defining named properties.
prop_named :: (Arbitrary t, Show t, Testable a)
           => String -> (FormatString s -> t -> a) -> String -> FormatString s -> NamedProperty
prop_named name prop typeName f = (name ++ " " ++ typeName ++ " " ++ show f, property (prop f))

prop_parse_format :: (Eq t, FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_parse_format (FormatString f) t = parse f (format f t) == Just t

prop_parse_format_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t) 
                           => String -> FormatString t -> NamedProperty
prop_parse_format_named = prop_named "prop_parse_format" prop_parse_format

-- Verify case-insensitivity with upper case.
prop_parse_format_upper :: (Eq t, FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_parse_format_upper (FormatString f) t = parse f (map toUpper $ format f t) == Just t

prop_parse_format_upper_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t) 
                              => String -> FormatString t -> NamedProperty
prop_parse_format_upper_named = prop_named "prop_parse_format_upper" prop_parse_format_upper

-- Verify case-insensitivity with lower case.
prop_parse_format_lower :: (Eq t, FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_parse_format_lower (FormatString f) t = parse f (map toLower $ format f t) == Just t

prop_parse_format_lower_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t) 
                              => String -> FormatString t -> NamedProperty
prop_parse_format_lower_named = prop_named "prop_parse_format_lower" prop_parse_format_lower

prop_format_parse_format :: (FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_format_parse_format (FormatString f) t = 
    fmap (format f) (parse f (format f t) `asTypeOf` Just t) == Just (format f t)

prop_format_parse_format_named :: (Arbitrary t, Show t, FormatTime t, ParseTime t) 
                                  => String -> FormatString t -> NamedProperty
prop_format_parse_format_named = prop_named "prop_format_parse_format" prop_format_parse_format

--
-- * crashes in parse
--

newtype Input = Input String

instance Show Input where
    show (Input s) = s

instance Arbitrary Input where
    arbitrary = liftM Input $ list cs
      where cs = elements (['0'..'9'] ++ ['-',' ','/'] ++ ['a'..'z'] ++ ['A' .. 'Z'])
            list g = sized (\n -> choose (0,n) >>= \l -> replicateM l g)
    coarbitrary (Input s) = coarbitrary (sum (map ord s))

prop_no_crash_bad_input :: (Eq t, ParseTime t) => FormatString t -> Input -> Property
prop_no_crash_bad_input fs@(FormatString f) (Input s) = property $ 
    case parse f s of
      Nothing -> True
      Just t  -> t == t `asTypeOf` formatType fs
  where 
prop_no_crash_bad_input_named :: (Eq t, ParseTime t)
                                 => String -> FormatString t -> NamedProperty
prop_no_crash_bad_input_named = prop_named "prop_no_crash_bad_input" prop_no_crash_bad_input

--
--
--

newtype FormatString a = FormatString String

formatType :: FormatString t -> t
formatType _ = undefined

castFormatString :: FormatString a -> FormatString b
castFormatString (FormatString f) = FormatString f

instance Show (FormatString a) where
    show (FormatString f) = show f

type NamedProperty = (String, Property)

properties :: [NamedProperty]
properties = 
    [("prop_fromMondayStartWeek", property prop_fromMondayStartWeek),
     ("prop_fromSundayStartWeek", property prop_fromSundayStartWeek)]
 ++ [("prop_read_show Day", property (prop_read_show :: Day -> Bool)),
     ("prop_read_show TimeOfDay", property (prop_read_show :: TimeOfDay -> Bool)),
     ("prop_read_show LocalTime", property (prop_read_show :: LocalTime -> Bool)),
     ("prop_read_show TimeZone", property (prop_read_show :: TimeZone -> Bool)),
     ("prop_read_show ZonedTime", property (prop_read_show :: ZonedTime -> Bool)),
     ("prop_read_show UTCTime", property (prop_read_show :: UTCTime -> Bool))]
 ++ [("prop_parse_showWeekDate", property prop_parse_showWeekDate),
     ("prop_parse_showGregorian", property prop_parse_showGregorian),
     ("prop_parse_showOrdinalDate", property prop_parse_showOrdinalDate)]

 ++ map (prop_parse_format_named "Day") dayFormats
 ++ map (prop_parse_format_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_named "UTCTime") utcTimeFormats

 ++ map (prop_parse_format_upper_named "Day") dayFormats
 ++ map (prop_parse_format_upper_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_upper_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_upper_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_upper_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_upper_named "UTCTime") utcTimeFormats

 ++ map (prop_parse_format_lower_named "Day") dayFormats
 ++ map (prop_parse_format_lower_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_lower_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_lower_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_lower_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_lower_named "UTCTime") utcTimeFormats

 ++ map (prop_format_parse_format_named "Day") partialDayFormats
 ++ map (prop_format_parse_format_named "TimeOfDay") partialTimeOfDayFormats
 ++ map (prop_format_parse_format_named "LocalTime") partialLocalTimeFormats
 ++ map (prop_format_parse_format_named "ZonedTime") partialZonedTimeFormats
 ++ map (prop_format_parse_format_named "UTCTime") partialUTCTimeFormats

 ++ map (prop_no_crash_bad_input_named "Day") (dayFormats ++ partialDayFormats ++ failingPartialDayFormats)
 ++ map (prop_no_crash_bad_input_named "TimeOfDay") (timeOfDayFormats ++ partialTimeOfDayFormats)
 ++ map (prop_no_crash_bad_input_named "LocalTime") (localTimeFormats ++ partialLocalTimeFormats)
 ++ map (prop_no_crash_bad_input_named "TimeZone") (timeZoneFormats)
 ++ map (prop_no_crash_bad_input_named "ZonedTime") (zonedTimeFormats ++ partialZonedTimeFormats)
 ++ map (prop_no_crash_bad_input_named "UTCTime") (utcTimeFormats ++ partialUTCTimeFormats)



dayFormats :: [FormatString Day]
dayFormats = map FormatString
    [
     -- numeric year, month, day
     "%Y-%m-%d","%Y%m%d","%C%y%m%d","%Y %m %e","%m/%d/%Y","%d/%m/%Y","%Y/%d/%m","%D %C","%F",
     -- month names
     "%Y-%B-%d","%Y-%b-%d","%Y-%h-%d",
     -- ordinal dates
     "%Y-%j",
     -- ISO week dates
     "%G-%V-%u","%G-%V-%a","%G-%V-%A","%G-%V-%w", "%A week %V, %G", "day %V, week %A, %G",
     "%G-W%V-%u",
     "%f%g-%V-%u","%f%g-%V-%a","%f%g-%V-%A","%f%g-%V-%w", "%A week %V, %f%g", "day %V, week %A, %f%g",
     "%f%g-W%V-%u",
     -- monday and sunday week dates
     "%Y-w%U-%A", "%Y-w%W-%A", "%Y-%A-w%U", "%Y-%A-w%W", "%A week %U, %Y", "%A week %W, %Y"
    ]

timeOfDayFormats :: [FormatString TimeOfDay]
timeOfDayFormats = map FormatString
    [
     -- 24 h formats
     "%H:%M:%S.%q","%k:%M:%S.%q","%H%M%S.%q","%T.%q","%X.%q","%R:%S.%q",
     "%H:%M:%S%Q","%k:%M:%S%Q","%H%M%S%Q","%T%Q","%X%Q","%R:%S%Q",
     -- 12 h formats
     "%I:%M:%S.%q %p","%I:%M:%S.%q %P","%l:%M:%S.%q %p","%r %q",
     "%I:%M:%S%Q %p","%I:%M:%S%Q %P","%l:%M:%S%Q %p","%r %Q"
    ]

localTimeFormats :: [FormatString LocalTime]
localTimeFormats = map FormatString $ 
  []
{-
  -- there's soo many of them...
  concat [ [df ++ " " ++ tf, tf ++ " " ++ df] | FormatString df <- dayFormats, 
                                                   FormatString tf <- timeOfDayFormats]
-}

timeZoneFormats :: [FormatString TimeZone]
timeZoneFormats = map FormatString ["%z","%z%Z","%Z%z","%Z"]

zonedTimeFormats :: [FormatString ZonedTime]
zonedTimeFormats = map FormatString
  ["%a, %d %b %Y %H:%M:%S.%q %z", "%a, %d %b %Y %H:%M:%S%Q %z", "%s.%q %z", "%s%Q %z",
   "%a, %d %b %Y %H:%M:%S.%q %Z", "%a, %d %b %Y %H:%M:%S%Q %Z", "%s.%q %Z", "%s%Q %Z"]

utcTimeFormats :: [FormatString UTCTime]
utcTimeFormats = map FormatString 
  ["%s.%q","%s%Q"]

--
-- * Formats that do not include all the information
--

partialDayFormats :: [FormatString Day]
partialDayFormats = map FormatString
    [ ]

partialTimeOfDayFormats :: [FormatString TimeOfDay]
partialTimeOfDayFormats = map FormatString
    [ ]

partialLocalTimeFormats :: [FormatString LocalTime]
partialLocalTimeFormats = map FormatString 
    [
     -- %c does not include second decimals 
     "%c" 
    ]

partialZonedTimeFormats :: [FormatString ZonedTime]
partialZonedTimeFormats = map FormatString 
    [
     -- %s does not include second decimals
     "%s %z",
     -- %S does not include second decimals
     "%c", "%a, %d %b %Y %H:%M:%S %Z"
    ]

partialUTCTimeFormats :: [FormatString UTCTime]
partialUTCTimeFormats = map FormatString 
    [
     -- %s does not include second decimals
     "%s",
     -- %c does not include second decimals
     "%c"
    ]


--
-- * Known failures
--

knownFailures :: [NamedProperty]
knownFailures =
    map (prop_format_parse_format_named "Day") failingPartialDayFormats

failingPartialDayFormats :: [FormatString Day]
failingPartialDayFormats = map FormatString
    [ -- ISO week dates with two digit year. 
      -- This can fail in the beginning or the end of a year where
      -- the ISO week date year does not match the gregorian year.
     "%g-%V-%u","%g-%V-%a","%g-%V-%A","%g-%V-%w", "%A week %V, %g", "day %V, week %A, %g",
     "%g-W%V-%u"
    ]
