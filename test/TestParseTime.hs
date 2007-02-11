{-# OPTIONS -Wall -Werror -fno-warn-type-defaults -fno-warn-unused-binds #-}

import Control.Monad
import Data.Char
import Data.Ratio
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Locale
import Test.QuickCheck


ntest :: Int
ntest = 1000

main :: IO ()
main = do putStrLn "Should work:"
          checkAll properties
          putStrLn "Known failures:"
          checkAll knownFailures

checkAll :: [NamedProperty] -> IO ()
checkAll ps = mapM_ (checkOne config) ps
 where config = defaultConfig { configMaxTest = ntest }
       
checkOne :: Config -> NamedProperty -> IO ()
checkOne config (n,p) = 
    do putStr (rpad 65 ' ' n)
       check config p
  where rpad n' c xs = xs ++ replicate (n' - length xs) c


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

prop_parse_format :: (Eq t, FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_parse_format (FormatString f) t = parse f (format f t) == Just t

prop_parse_format_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t) 
                           => String -> FormatString t -> NamedProperty
prop_parse_format_named typeName f = 
    ("prop_parse_format " ++ typeName ++ " " ++ show f, 
     property (prop_parse_format f))

prop_format_parse_format :: (FormatTime t, ParseTime t) => FormatString t -> t -> Bool
prop_format_parse_format (FormatString f) t = 
    fmap (format f) (parse f (format f t) `asTypeOf` Just t) == Just (format f t)

prop_format_parse_format_named :: (Arbitrary t, Show t, FormatTime t, ParseTime t) 
                                  => String -> FormatString t -> NamedProperty
prop_format_parse_format_named typeName f = 
    ("prop_format_parse_format " ++ typeName ++ " " ++ show f, 
     property (prop_format_parse_format f))

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
prop_no_crash_bad_input_named typeName f = 
    ("prop_no_crash_bad_input " ++ typeName ++ " " ++ show f, 
     property (prop_no_crash_bad_input f))

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
