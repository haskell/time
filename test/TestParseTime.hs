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
  where rpad n c xs = xs ++ replicate (n - length xs) c

parse f t = parseTime defaultTimeLocale f t

format f t = formatTime defaultTimeLocale f t


instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
        where intSecs = liftM secondsToDiffTime $ choose (0, 86400) 
              fracSecs = liftM picosecondsToDiffTime $ choose (0, 86400 * 10^12) 
              secondsToDiffTime :: Integer -> DiffTime
              secondsToDiffTime = fromInteger
              picosecondsToDiffTime :: Integer -> DiffTime
              picosecondsToDiffTime x = fromRational (x % 10^12)
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

prop_fromMondayStartWeek d = 
    let (w,wd)  = mondayStartWeek d
        (y,_,_) = toGregorian d
     in fromMondayStartWeek y w wd == d

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

 ++ map (prop_no_crash_bad_input_named "Day") dayFormats
 ++ map (prop_no_crash_bad_input_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_no_crash_bad_input_named "LocalTime") localTimeFormats
 ++ map (prop_no_crash_bad_input_named "TimeZone") timeZoneFormats
 ++ map (prop_no_crash_bad_input_named "ZonedTime") zonedTimeFormats
 ++ map (prop_no_crash_bad_input_named "UTCTime") utcTimeFormats




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
     -- monday and sunday week dates
     "%Y-w%U-%A", "%Y-w%W-%A", "%Y-%A-w%U", "%Y-%A-w%W", "%A week %U, %Y", "%A week %W, %Y"
    ]

timeOfDayFormats :: [FormatString TimeOfDay]
timeOfDayFormats = map FormatString
    [
     -- 24 h formats
     "%H:%M:%S","%k:%M:%S","%H%M%S","%T","%X","%R:%S",
     -- 12 h formats
     "%I:%M:%S %p","%I:%M:%S %P","%l:%M:%S %p","%r"
    ]

localTimeFormats :: [FormatString LocalTime]
localTimeFormats = map FormatString $ 
  ["%c"]
{-
  -- there's soo many of them...
  concat [ [df ++ " " ++ tf, tf ++ " " ++ df] | FormatString df <- dayFormats, 
                                                   FormatString tf <- timeOfDayFormats]
-}

timeZoneFormats :: [FormatString TimeZone]
timeZoneFormats = map FormatString ["%z","%z%Z","%Z%z"]

zonedTimeFormats :: [FormatString ZonedTime]
zonedTimeFormats = map FormatString
  ["%a, %d %b %Y %H:%M:%S %z"]

utcTimeFormats :: [FormatString UTCTime]
utcTimeFormats = map FormatString 
  ["%c"]

--
-- * Known failures
--

knownFailures :: [NamedProperty]
knownFailures =
    map (prop_parse_format_named "Day") failingDayFormats
 ++ map (prop_parse_format_named "LocalTime") failingLocalTimeFormats
 ++ map (prop_parse_format_named "TimeZone") failingTimeZoneFormats
 ++ map (prop_parse_format_named "ZonedTime") failingZonedTimeFormats
 ++ map (prop_parse_format_named "UTCTime") failingUTCTimeFormats



failingDayFormats :: [FormatString Day]
failingDayFormats = map FormatString
    [ ]

failingLocalTimeFormats :: [FormatString LocalTime]
failingLocalTimeFormats = map FormatString 
    [ ]

failingTimeZoneFormats :: [FormatString TimeZone]
failingTimeZoneFormats = map FormatString 
    [
     -- %Z does not figure out the offset
     "%Z"
    ]

failingZonedTimeFormats :: [FormatString ZonedTime]
failingZonedTimeFormats = map FormatString 
    [
     -- can't figure out offset from %Z, also, formatTime produces "" for %Z
     "%c",
     "%a, %d %b %Y %H:%M:%S %Z",
     -- %s does not include second decimals
     "%s %z"
    ]

failingUTCTimeFormats :: [FormatString UTCTime]
failingUTCTimeFormats = map FormatString 
    [
     -- %s does not include second decimals
     "%s"
    ]

