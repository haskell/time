{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"

-- #hide
module Data.Time.Format.Parse 
    (
    -- * UNIX-style parsing
#if LANGUAGE_Rank2Types
    parseTime, readTime, readsTime,
#endif
    ParseTime(..)
    ) where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

#if LANGUAGE_Rank2Types
import Control.Monad
#endif
import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import System.Locale
#if LANGUAGE_Rank2Types
import Text.ParserCombinators.ReadP hiding (char, string)
#endif

#if LANGUAGE_Rank2Types
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.char'.
char :: Char -> ReadP Char
char c = satisfy (\x -> toUpper c == toUpper x)
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.string'.
string :: String -> ReadP String
string this = do s <- look; scan this s
  where
    scan []     _                               = do return this
    scan (x:xs) (y:ys) | toUpper x == toUpper y = do _ <- get; scan xs ys
    scan _      _                               = do pfail
#endif
-- | Convert string to upper case.
up :: String -> String
up = map toUpper


-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the 
                                 -- corresponding part of the input.
              -> t

#if LANGUAGE_Rank2Types
-- | Parses a time value given a format string.
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers.
-- Leading and trailing whitespace is accepted. Case is not significant.
-- Some variations in the input are accepted:
--
-- [@%z@] accepts any of @-HHMM@ or @-HH:MM@.
--
-- [@%Z@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime l fmt s = case goodReadsTime l fmt s of
    [t] -> Just t
    _   -> Nothing

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTime' for details.
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime l fmt s = case goodReadsTime l fmt s of
    [t] -> t
    []  -> error $ "readTime: no parse of " ++ show s
    _   -> error $ "readTime: multiple parses of " ++ show s

goodReadsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> String     -- ^ Input string.
          -> [t]
goodReadsTime l fmt s = [t | (t,r) <- readsTime l fmt s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTime' for details.
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime l f = readP_to_S (liftM (buildTime l) r)
  where r = skipSpaces >> parseInput l (parseFormat l f)

--
-- * Internals
--

data Padding = NoPadding | SpacePadding | ZeroPadding
  deriving Show

type DateFormat = [DateFormatSpec]

data DateFormatSpec = Value (Maybe Padding) Char
                     | WhiteSpace
                     | Literal Char
  deriving Show

parseFormat :: TimeLocale -> String -> DateFormat
parseFormat l = p
  where p "" = []
        p ('%': '-' : c :cs) = (pc (Just NoPadding) c) ++ p cs
        p ('%': '_' : c :cs) = (pc (Just SpacePadding) c) ++ p cs
        p ('%': '0' : c :cs) = (pc (Just ZeroPadding) c) ++ p cs
        p ('%': c :cs) = (pc Nothing c) ++ p cs
        p (c:cs) | isSpace c = WhiteSpace : p cs
        p (c:cs) = Literal c : p cs
        pc _ 'c' = p (dateTimeFmt l)
        pc _ 'R' = p "%H:%M"
        pc _ 'T' = p "%H:%M:%S"
        pc _ 'X' = p (timeFmt l)
        pc _ 'r' = p (time12Fmt l)
        pc _ 'D' = p "%m/%d/%y"
        pc _ 'F' = p "%Y-%m-%d"
        pc _ 'x' = p (dateFmt l)
        pc _ 'h' = p "%b"
        pc _ '%' = [Literal '%']
        pc mpad c   = [Value mpad c]

parseInput :: TimeLocale -> DateFormat -> ReadP [(Char,String)]
parseInput _ [] = return []
parseInput l (Value mpad c:ff) = do
  s <- parseValue l mpad c
  r <- parseInput l ff
  return ((c,s):r)
parseInput l (Literal c:ff) = do
  _ <- char c
  parseInput l ff
parseInput l (WhiteSpace:ff) = do
  _ <- satisfy isSpace
  case ff of
     (WhiteSpace:_) -> return ()
     _ -> skipSpaces
  parseInput l ff

-- | Get the string corresponding to the given format specifier.
parseValue :: TimeLocale -> Maybe Padding -> Char -> ReadP String
parseValue l mpad c = 
    case c of
      -- century
      'C' -> digits SpacePadding 2
      'f' -> digits SpacePadding 2

      -- year
      'Y' -> digits SpacePadding 4
      'G' -> digits SpacePadding 4
      
      -- year of century
      'y' -> digits ZeroPadding 2
      'g' -> digits ZeroPadding 2

      -- month of year
      'B' -> oneOf (map fst (months l))
      'b' -> oneOf (map snd (months l))
      'm' -> digits ZeroPadding 2
      
      -- day of month
      'd' -> digits ZeroPadding 2
      'e' -> digits SpacePadding 2
      
      -- week of year
      'V' -> digits ZeroPadding 2
      'U' -> digits ZeroPadding 2
      'W' -> digits ZeroPadding 2
      
      -- day of week
      'u' -> oneOf $ map (:[]) ['1'..'7']
      'a' -> oneOf (map snd (wDays l))
      'A' -> oneOf (map fst (wDays l))
      'w' -> oneOf $ map (:[]) ['0'..'6']
      
      -- day of year
      'j' -> digits ZeroPadding 3

      -- dayhalf of day (i.e. AM or PM)
      'P' -> oneOf (let (am,pm) = amPm l in [am, pm])
      'p' -> oneOf (let (am,pm) = amPm l in [am, pm])

      -- hour of day (i.e. 24h)
      'H' -> digits ZeroPadding 2
      'k' -> digits SpacePadding 2
      
      -- hour of dayhalf (i.e. 12h)
      'I' -> digits ZeroPadding 2
      'l' -> digits SpacePadding 2
      
      -- minute of hour
      'M' -> digits ZeroPadding 2
      
      -- second of minute
      'S' -> digits ZeroPadding 2
      
      -- picosecond of second
      'q' -> digits ZeroPadding 12
      'Q' -> liftM2 (:) (char '.') (munch isDigit) <++ return ""

      -- time zone
      'z' -> numericTZ
      'Z' -> munch1 isAlpha <++
             numericTZ <++
             return "" -- produced by %Z for LocalTime

      -- seconds since epoch
      's' -> (char '-' >> liftM ('-':) (munch1 isDigit)) 
             <++ munch1 isDigit

      _   -> fail $ "Unknown format character: " ++ show c
  where
    oneOf = choice . map string
    digitsforce ZeroPadding n = count n (satisfy isDigit)
    digitsforce SpacePadding _n = skipSpaces >> many1 (satisfy isDigit)
    digitsforce NoPadding _n = many1 (satisfy isDigit)
    digits pad = digitsforce (fromMaybe pad mpad)
    numericTZ = do s <- choice [char '+', char '-']
                   h <- digitsforce ZeroPadding 2
                   optional (char ':')
                   m <- digitsforce ZeroPadding 2
                   return (s:h++m)
#endif

--
-- * Instances for the time package types
--

data DayComponent = Century Integer -- century of all years
                  | CenturyYear Integer -- 0-99, last two digits of both real years and week years
                  | YearMonth Int -- 1-12
                  | MonthDay Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | YearWeek WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    buildTime l = buildDay . concatMap (uncurry f)
     where
      f c x = 
        case c of
          -- %C: century (all but the last two digits of the year), 00 - 99
          'C' -> [Century (read x)]
          -- %f century (all but the last two digits of the year), 00 - 99
          'f' -> [Century (read x)]
          -- %Y: year
          'Y' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %G: year for Week Date format
          'G' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %y: last two digits of year, 00 - 99
          'y' -> [CenturyYear (read x)]
          -- %g: last two digits of year for Week Date format, 00 - 99
          'g' -> [CenturyYear (read x)]
          -- %B: month name, long form (fst from months locale), January - December
          'B' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . fst) (months l))))]
          -- %b: month name, short form (snd from months locale), Jan - Dec
          'b' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . snd) (months l))))]
          -- %m: month of year, leading 0 as needed, 01 - 12
          'm' -> [YearMonth (read x)]
          -- %d: day of month, leading 0 as needed, 01 - 31
          'd' -> [MonthDay (read x)]
          -- %e: day of month, leading space as needed, 1 - 31
          'e' -> [MonthDay (read x)]
          -- %V: week for Week Date format, 01 - 53
          'V' -> [YearWeek ISOWeek (read x)]
          -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 01 - 53
          'U' -> [YearWeek SundayWeek (read x)]
          -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 01 - 53
          'W' -> [YearWeek MondayWeek (read x)]
          -- %u: day for Week Date format, 1 - 7
          'u' -> [WeekDay (read x)]
          -- %a: day of week, short form (snd from wDays locale), Sun - Sat
          'a' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . snd) (wDays l))) + 6) `mod` 7)]
          -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
          'A' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . fst) (wDays l))) + 6) `mod` 7)]
          -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
          'w' -> [WeekDay (((read x + 6) `mod` 7) + 1)]
          -- %j: day of year for Ordinal Date format, 001 - 366
          'j' -> [YearDay (read x)]
          _   -> []

      buildDay cs = rest cs
        where
        y = let 
                d = safeLast 70 [x | CenturyYear x <- cs]
                c = safeLast (if d >= 69 then 19 else 20) [x | Century x <- cs]
             in 100 * c + d

        rest (YearMonth m:_)  = let d = safeLast 1 [x | MonthDay x <- cs]
                             in fromGregorian y m d
        rest (YearDay d:_) = fromOrdinalDate y d
        rest (YearWeek wt w:_) = let d = safeLast 4 [x | WeekDay x <- cs]
                              in case wt of
                                   ISOWeek    -> fromWeekDate y w d
                                   SundayWeek -> fromSundayStartWeek y w (d `mod` 7)
                                   MondayWeek -> fromMondayStartWeek y w d
        rest (_:xs)        = rest xs
        rest []            = rest [YearMonth 1]

      safeLast x xs = last (x:xs)

instance ParseTime TimeOfDay where
    buildTime l = foldl f midnight
        where
          f t@(TimeOfDay h m s) (c,x) = 
              case c of
                'P' -> if up x == fst (amPm l) then am else pm
                'p' -> if up x == fst (amPm l) then am else pm
                'H' -> TimeOfDay (read x) m s
                'I' -> TimeOfDay (read x) m s
                'k' -> TimeOfDay (read x) m s
                'l' -> TimeOfDay (read x) m s
                'M' -> TimeOfDay h (read x) s
                'S' -> TimeOfDay h m (fromInteger (read x))
                'q' -> TimeOfDay h m (mkPico (truncate s) (read x))
                'Q' -> if null x then t 
                        else let ps = read $ take 12 $ rpad 12 '0' $ drop 1 x
                              in TimeOfDay h m (mkPico (truncate s) ps)
                _   -> t
            where am = TimeOfDay (h `mod` 12) m s
                  pm = TimeOfDay (if h < 12 then h + 12 else h) m s

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    buildTime l xs = LocalTime (buildTime l xs) (buildTime l xs)

instance ParseTime TimeZone where
    buildTime _ = foldl f (minutesToTimeZone 0)
      where 
        f t@(TimeZone offset dst name) (c,x) = 
            case c of
              'z' -> zone
              'Z' | null x           -> t
                  | isAlpha (head x) -> let y = up x in
                      case lookup y _TIMEZONES_ of
                        Just (offset', dst') -> TimeZone offset' dst' y
                        Nothing -> TimeZone offset dst y
                  | otherwise        -> zone
              _   -> t
          where zone = TimeZone (readTzOffset x) dst name

instance ParseTime ZonedTime where
    buildTime l xs = foldl f (ZonedTime (buildTime l xs) (buildTime l xs)) xs
        where
          f t@(ZonedTime (LocalTime _ tod) z) (c,x) =
              case c of
                's' -> let s = fromInteger (read x)
                           (_,ps) = properFraction (todSec tod) :: (Integer,Pico)
                           s' = s + fromRational (toRational ps)
                        in utcToZonedTime z (posixSecondsToUTCTime s')
                _   -> t

instance ParseTime UTCTime where
    buildTime l = zonedTimeToUTC . buildTime l

-- * Read instances for time package types

#if LANGUAGE_Rank2Types
instance Read Day where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read TimeZone where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%Z"

instance Read ZonedTime where
    readsPrec n = readParen False $ \s ->
        [(ZonedTime t z, r2) | (t,r1) <- readsPrec n s, (z,r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = [ (zonedTimeToUTC t, r) | (t,r) <- readsPrec n s ]
#endif

readTzOffset :: String -> Int
readTzOffset str =
    case str of
      (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
      (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
      _ -> 0
    where calc s h1 h2 m1 m2 = sign * (60 * h + m)
              where sign = if s == '-' then -1 else 1
                    h = read [h1,h2]
                    m = read [m1,m2]

-- Dubious
_TIMEZONES_ :: [(String, (Int, Bool))]
_TIMEZONES_ =
    -- New Zealand Daylight-Saving Time
    [("NZDT",    (readTzOffset "+13:00", True))
    -- International Date Line, East
    ,("IDLE",    (readTzOffset "+12:00", False))
    -- New Zealand Standard Time
    ,("NZST",    (readTzOffset "+12:00", False))
    -- New Zealand Time
    ,("NZT",     (readTzOffset "+12:00", False))
    -- Australia Eastern Summer Standard Time
    ,("AESST",   (readTzOffset "+11:00", False))
    -- Central Australia Summer Standard Time
    ,("ACSST",   (readTzOffset "+10:30", False))
    -- Central Australia Daylight-Saving Time
    ,("CADT",    (readTzOffset "+10:30", True))
    -- South Australian Daylight-Saving Time
    ,("SADT",    (readTzOffset "+10:30", True))
    -- Australia Eastern Standard Time
    ,("AEST",    (readTzOffset "+10:00", False))
    -- East Australian Standard Time
    ,("EAST",    (readTzOffset "+10:00", False))
    -- Guam Standard Time, Russia zone 9
    ,("GST",     (readTzOffset "+10:00", False))
    -- Melbourne, Australia
    ,("LIGT",    (readTzOffset "+10:00", False))
    -- South Australia Standard Time
    ,("SAST",    (readTzOffset "+09:30", False))
    -- Central Australia Standard Time
    ,("CAST",    (readTzOffset "+09:30", False))
    -- Australia Western Summer Standard Time
    ,("AWSST",   (readTzOffset "+09:00", False))
    -- Japan Standard Time, Russia zone 8
    ,("JST",     (readTzOffset "+09:00", False))
    -- Korea Standard Time
    ,("KST",     (readTzOffset "+09:00", False))
    -- Kwajalein Time
    ,("MHT",     (readTzOffset "+09:00", False))
    -- West Australian Daylight-Saving Time
    ,("WDT",     (readTzOffset "+09:00", True))
    -- Moluccas Time
    ,("MT",      (readTzOffset "+08:30", False))
    -- Australia Western Standard Time
    ,("AWST",    (readTzOffset "+08:00", False))
    -- China Coastal Time
    ,("CCT",     (readTzOffset "+08:00", False))
    -- West Australian Daylight-Saving Time
    ,("WADT",    (readTzOffset "+08:00", True))
    -- West Australian Standard Time
    ,("WST",     (readTzOffset "+08:00", False))
    -- Java Time
    ,("JT",      (readTzOffset "+07:30", False))
    -- Almaty Summer Time
    ,("ALMST",   (readTzOffset "+07:00", False))
    -- West Australian Standard Time
    ,("WAST",    (readTzOffset "+07:00", False))
    -- Christmas (Island) Time
    ,("CXT",     (readTzOffset "+07:00", False))
    -- Myanmar Time
    ,("MMT",     (readTzOffset "+06:30", False))
    -- Almaty Time
    ,("ALMT",    (readTzOffset "+06:00", False))
    -- Mawson (Antarctica) Time
    ,("MAWT",    (readTzOffset "+06:00", False))
    -- Indian Chagos Time
    ,("IOT",     (readTzOffset "+05:00", False))
    -- Maldives Island Time
    ,("MVT",     (readTzOffset "+05:00", False))
    -- Kerguelen Time
    ,("TFT",     (readTzOffset "+05:00", False))
    -- Afghanistan Time
    ,("AFT",     (readTzOffset "+04:30", False))
    -- Antananarivo Summer Time
    ,("EAST",    (readTzOffset "+04:00", False))
    -- Mauritius Island Time
    ,("MUT",     (readTzOffset "+04:00", False))
    -- Reunion Island Time
    ,("RET",     (readTzOffset "+04:00", False))
    -- Mahe Island Time
    ,("SCT",     (readTzOffset "+04:00", False))
    -- Iran Time
    ,("IRT",     (readTzOffset "+03:30", False))
    -- Iran Time
    ,("IT",      (readTzOffset "+03:30", False))
    -- Antananarivo, Comoro Time
    ,("EAT",     (readTzOffset "+03:00", False))
    -- Baghdad Time
    ,("BT",      (readTzOffset "+03:00", False))
    -- Eastern Europe Daylight-Saving Time
    ,("EETDST",  (readTzOffset "+03:00", True))
    -- Hellas Mediterranean Time (?)
    ,("HMT",     (readTzOffset "+03:00", False))
    -- British Double Summer Time
    ,("BDST",    (readTzOffset "+02:00", False))
    -- Central European Summer Time
    ,("CEST",    (readTzOffset "+02:00", False))
    -- Central European Daylight-Saving Time
    ,("CETDST",  (readTzOffset "+02:00", True))
    -- Eastern European Time, Russia zone 1
    ,("EET",     (readTzOffset "+02:00", False))
    -- French Winter Time
    ,("FWT",     (readTzOffset "+02:00", False))
    -- Israel Standard Time
    ,("IST",     (readTzOffset "+02:00", False))
    -- Middle European Summer Time
    ,("MEST",    (readTzOffset "+02:00", False))
    -- Middle Europe Daylight-Saving Time
    ,("METDST",  (readTzOffset "+02:00", True))
    -- Swedish Summer Time
    ,("SST",     (readTzOffset "+02:00", False))
    -- British Summer Time
    ,("BST",     (readTzOffset "+01:00", False))
    -- Central European Time
    ,("CET",     (readTzOffset "+01:00", False))
    -- Dansk Normal Tid
    ,("DNT",     (readTzOffset "+01:00", False))
    -- French Summer Time
    ,("FST",     (readTzOffset "+01:00", False))
    -- Middle European Time
    ,("MET",     (readTzOffset "+01:00", False))
    -- Middle European Winter Time
    ,("MEWT",    (readTzOffset "+01:00", False))
    -- Mitteleuropaeische Zeit
    ,("MEZ",     (readTzOffset "+01:00", False))
    -- Norway Standard Time
    ,("NOR",     (readTzOffset "+01:00", False))
    -- Seychelles Time
    ,("SET",     (readTzOffset "+01:00", False))
    -- Swedish Winter Time
    ,("SWT",     (readTzOffset "+01:00", False))
    -- Western European Daylight-Saving Time
    ,("WETDST",  (readTzOffset "+01:00", True))
    --  Greenwich Mean Time
    ,("GMT",     (readTzOffset "+00:00", False))
    --  Universal Time
    ,("UT",      (readTzOffset "+00:00", False))
    --  Universal Coordinated Time
    ,("UTC",     (readTzOffset "+00:00", False))
    --  Same as UTC
    ,("Z",       (readTzOffset "+00:00", False))
    --  Same as UTC
    ,("ZULU",    (readTzOffset "+00:00", False))
    --  Western European Time
    ,("WET",     (readTzOffset "+00:00", False))
    -- West Africa Time
    ,("WAT",     (readTzOffset "-01:00", False))
    -- Fernando de Noronha Summer Time
    ,("FNST",    (readTzOffset "-01:00", False))
    -- Fernando de Noronha Time
    ,("FNT",     (readTzOffset "-02:00", False))
    -- Brasilia Summer Time
    ,("BRST",    (readTzOffset "-02:00", False))
    -- Newfoundland Daylight-Saving Time
    ,("NDT",     (readTzOffset "-02:30", True))
    -- Atlantic Daylight-Saving Time
    ,("ADT",     (readTzOffset "-03:00", True))
    -- (unknown)
    ,("AWT",     (readTzOffset "-03:00", False))
    -- Brasilia Time
    ,("BRT",     (readTzOffset "-03:00", False))
    -- Newfoundland Standard Time
    ,("NFT",     (readTzOffset "-03:30", False))
    -- Newfoundland Standard Time
    ,("NST",     (readTzOffset "-03:30", False))
    -- Atlantic Standard Time (Canada)
    ,("AST",     (readTzOffset "-04:00", False))
    -- Atlantic/Porto Acre Summer Time
    ,("ACST",    (readTzOffset "-04:00", False))
    -- Eastern Daylight-Saving Time
    ,("EDT",     (readTzOffset "-04:00", True))
    -- Atlantic/Porto Acre Standard Time
    ,("ACT",     (readTzOffset "-05:00", False))
    -- Central Daylight-Saving Time
    ,("CDT",     (readTzOffset "-05:00", True))
    -- Eastern Standard Time
    ,("EST",     (readTzOffset "-05:00", False))
    -- Central Standard Time
    ,("CST",     (readTzOffset "-06:00", False))
    -- Mountain Daylight-Saving Time
    ,("MDT",     (readTzOffset "-06:00", True))
    -- Mountain Standard Time
    ,("MST",     (readTzOffset "-07:00", False))
    -- Pacific Daylight-Saving Time
    ,("PDT",     (readTzOffset "-07:00", True))
    -- Alaska Daylight-Saving Time
    ,("AKDT",    (readTzOffset "-08:00", True))
    -- Pacific Standard Time
    ,("PST",     (readTzOffset "-08:00", False))
    -- Yukon Daylight-Saving Time
    ,("YDT",     (readTzOffset "-08:00", True))
    -- Alaska Standard Time
    ,("AKST",    (readTzOffset "-09:00", False))
    -- Hawaii/Alaska Daylight-Saving Time
    ,("HDT",     (readTzOffset "-09:00", True))
    -- Yukon Standard Time
    ,("YST",     (readTzOffset "-09:00", False))
    -- Marquesas Time
    ,("MART",    (readTzOffset "-09:30", False))
    -- Alaska/Hawaii Standard Time
    ,("AHST",    (readTzOffset "-10:00", False))
    -- Hawaii Standard Time
    ,("HST",     (readTzOffset "-10:00", False))
    -- Central Alaska Time
    ,("CAT",     (readTzOffset "-10:00", False))
    -- Nome Time
    ,("NT",      (readTzOffset "-11:00", False))
    -- International Date Line, West
    ,("IDLW",    (readTzOffset "-12:00", False))
    ]
