{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.Parse 
    (
    -- * UNIX-style parsing
    parseTime, readTime, readsTime,
    ParseTime(..)
    ) where

import Data.Time.Clock.POSIX
import Data.Time.Clock.UTC
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime.LocalTime
import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone

import Control.Monad
import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import System.Locale
import Text.ParserCombinators.ReadP

-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the 
                                 -- corresponding part of the input.
              -> t

-- | Parse a time value given a format string. Supports the same %-codes as
-- 'formatTime'.
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime l fmt s = case readsTime l fmt s of
                      [(t,r)] | all isSpace r -> Just t
                      _        -> Nothing

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. Supports the same %-codes as
-- 'formatTime'.
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime l fmt s = case readsTime l fmt s of
                      [(t,r)] | all isSpace r -> t
                      [(_,x)]  -> error $ "readTime: junk at end of " ++ show x
                      _        -> error $ "readsTime: bad input " ++ show s

-- | Parse a time value given a format string. Supports the same %-codes as
-- 'formatTime'.
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime l f = readP_to_S (liftM (buildTime l) r)
  where r = skipSpaces >> parseInput l (parseFormat l f)

--
-- * Internals
--

type DateFormat = [DateFormatSpec]

data DateFormatSpec = Value Char
                     | WhiteSpace
                     | Literal Char
  deriving Show

parseFormat :: TimeLocale -> String -> DateFormat
parseFormat l = p
  where p "" = []
        p ('%': c :cs) = s ++ p cs
            where s = case c of
                        'c' -> p (dateTimeFmt l)
                        'R' -> p "%H:%M"
                        'T' -> p "%H:%M:%S"
                        'X' -> p (timeFmt l)
                        'r' -> p (time12Fmt l)
                        'D' -> p "%m/%d/%y"
                        'F' -> p "%Y-%m-%d"
                        'x' -> p (dateFmt l)
                        'h' -> p "%b"
                        '%' -> [Literal '%']
                        _   -> [Value c]
        p (c:cs) | isSpace c = WhiteSpace : p cs
        p (c:cs) = Literal c : p cs

parseInput :: TimeLocale -> DateFormat -> ReadP [(Char,String)]
parseInput l = liftM catMaybes . mapM p
  where p (Value c)   = parseValue l c >>= return . Just . (,) c
        p WhiteSpace  = skipSpaces >> return Nothing
        p (Literal c) = char c >> return Nothing

-- | Get the string corresponding to the given format specifier.
parseValue :: TimeLocale -> Char -> ReadP String
parseValue l c = 
    case c of
      'z' -> liftM2 (:) (choice [char '+', char '-']) (digits 4)
      'Z' -> munch isUpper
      'P' -> oneOf (let (am,pm) = amPm l 
                     in [map toLower am, map toLower pm])
      'p' -> oneOf (let (am,pm) = amPm l in [am, pm])
      'H' -> digits 2
      'I' -> digits 2
      'k' -> spdigits 2
      'l' -> spdigits 2
      'M' -> digits 2 
      'S' -> do s <- digits 2
                ds <- liftM2 (:) (char '.') (munch isDigit)
                      <++ return ""
                return $ s ++ ds
      's' -> (char '-' >> liftM ('-':) (munch1 isDigit)) 
             <++ munch1 isDigit
      'Y' -> digits 4
      'y' -> digits 2
      'C' -> digits 2
      'B' -> oneOf (map fst (months l))
      'b' -> oneOf (map snd (months l))
      'm' -> digits 2
      'd' -> digits 2
      'e' -> spdigits 2
      'j' -> digits 3
      'G' -> digits 4
      'g' -> digits 2
      'V' -> digits 2
      'u' -> oneOf $ map (:[]) ['1'..'7']
      'a' -> oneOf (map snd (wDays l))
      'A' -> oneOf (map fst (wDays l))
      'U' -> digits 2
      'w' -> oneOf $ map (:[]) ['0'..'6']
      'W' -> digits 2
      _   -> fail $ "Unknown format character: " ++ show c
  where
    oneOf = choice . map string
    digits n = count n (satisfy isDigit)
    spdigits n = skipSpaces >> upTo n (satisfy isDigit)
    upTo :: Int -> ReadP a -> ReadP [a]
    upTo 0 _ = return []
    upTo n x = liftM2 (:) x (upTo (n-1) x) <++ return []



--
-- * Instances for the time package types
--

data DayComponent = Year Integer -- 0-99, last two digits of both real years and week years
                  | Century Integer -- century of all years
                  | Month Int -- 1-12
                  | Day Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | Week WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    buildTime l = buildDay . concatMap (uncurry f)
     where
      f c x = 
        case c of
          -- %Y: year
          'Y' -> let y = read x in [Century (y `div` 100), Year (y `mod` 100)]
          -- %y: last two digits of year, 00 - 99
          'y' -> [Year (read x)]
          -- %C: century (being the first two digits of the year), 00 - 99
          'C' -> [Century (read x)]
          -- %B: month name, long form (fst from months locale), January - December
          'B' -> [Month (1 + fromJust (elemIndex x (map fst (months l))))]
          -- %b: month name, short form (snd from months locale), Jan - Dec
          'b' -> [Month (1 + fromJust (elemIndex x (map snd (months l))))]
          -- %m: month of year, leading 0 as needed, 01 - 12
          'm' -> [Month (read x)]
          -- %d: day of month, leading 0 as needed, 01 - 31
          'd' -> [Day (read x)]
          -- %e: day of month, leading space as needed, 1 - 31
          'e' -> [Day (read x)]
          -- %j: day of year for Ordinal Date format, 001 - 366
          'j' -> [YearDay (read x)]
          -- %G: year for Week Date format
          'G' -> let y = read x in [Century (y `div` 100), Year (y `mod` 100)]
          -- %g: last two digits of year for Week Date format, 00 - 99
          'g' -> [Year (read x)]
          -- %V: week for Week Date format, 01 - 53
          'V' -> [Week ISOWeek (read x)]
          -- %u: day for Week Date format, 1 - 7
          'u' -> [WeekDay (read x)]
          -- %a: day of week, short form (snd from wDays locale), Sun - Sat
          'a' -> [WeekDay (1 + (fromJust (elemIndex x (map snd (wDays l))) + 6) `mod` 7)]
          -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
          'A' -> [WeekDay (1 + (fromJust (elemIndex x (map fst (wDays l))) + 6) `mod` 7)]
          -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 01 - 53
          'U' -> [Week SundayWeek (read x)]
          -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
          'w' -> [WeekDay (((read x + 6) `mod` 7) + 1)]
          -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 01 - 53
          'W' -> [Week MondayWeek (read x)]
          _   -> []

      buildDay cs = rest cs
        where
        y = let c = safeLast 19 [x | Century x <- cs]
                d = safeLast 70 [x | Year x <- cs]
             in 100 * c + d

        rest (Month m:_)  = let d = safeLast 1 [x | Day x <- cs]
                             in fromGregorian y m d
        rest (YearDay d:_) = fromOrdinalDate y d
        rest (Week wt w:_) = let d = safeLast 4 [x | WeekDay x <- cs]
                              in case wt of
                                   ISOWeek    -> fromWeekDate y w d
                                   SundayWeek -> fromSundayStartWeek y w (d `mod` 7)
                                   MondayWeek -> fromMondayStartWeek y w d
        rest (_:xs)        = rest xs
        rest []            = rest [Month 1]

      safeLast x xs = last (x:xs)

instance ParseTime TimeOfDay where
    buildTime l = foldl f midnight
        where
          f t@(TimeOfDay h m s) (c,x) = 
              case c of
                'P' -> if x == map toLower (fst (amPm l)) then am else pm
                'p' -> if x ==              fst (amPm l)  then am else pm
                'H' -> TimeOfDay (read x) m s
                'I' -> TimeOfDay (read x) m s
                'k' -> TimeOfDay (read x) m s
                'l' -> TimeOfDay (read x) m s
                'M' -> TimeOfDay h (read x) s
                'S' -> TimeOfDay h m (readFixed x)
                _   -> t
            where am = TimeOfDay (h `mod` 12) m s
                  pm = TimeOfDay (if h < 12 then h + 12 else h) m s


readFixed :: HasResolution a => String -> Fixed a
readFixed s = case break (=='.') s of
                (x,"")  -> fromInteger (read x)
                (x,_:y) -> mkFixed12 (read x) (read (rpad 12 '0' y))
  where rpad n c xs = xs ++ replicate (n - length xs) c
        mkFixed12 i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    buildTime l xs = LocalTime (buildTime l xs) (buildTime l xs)

instance ParseTime TimeZone where
    buildTime _ = foldl f (minutesToTimeZone 0)
      where 
        f t@(TimeZone offset dst name) (c,x) = 
            case c of
              'z' -> TimeZone (sign * (60 * h + m)) dst name
                  where (s:h1:h2:m1:m2:[]) = x
                        sign = if s == '-' then -1 else 1
                        h = read [h1,h2]
                        m = read [m1,m2] 
              'Z' -> TimeZone offset dst x -- FIXME: figure out timezone offset?
              _   -> t

instance ParseTime ZonedTime where
    buildTime l xs = foldl f (ZonedTime (buildTime l xs) (buildTime l xs)) xs
        where
          f t (c,x) =
              case c of
                's' -> utcToZonedTime (zonedTimeZone t) (posixSecondsToUTCTime (fromInteger (read x)))
                _   -> t

instance ParseTime UTCTime where
    buildTime l = zonedTimeToUTC . buildTime l

-- * Read instances for time package types

instance Read Day where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%H:%M:%S"

instance Read LocalTime where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

instance Read TimeZone where
    readsPrec _ = readParen False $ \s ->
                      readsTime defaultTimeLocale "%z" s
                       ++ readsTime defaultTimeLocale "%Z" s

instance Read ZonedTime where
    readsPrec n = readParen False $ \s ->
        [(ZonedTime t z, r2) | (t,r1) <- readsPrec n s, (z,r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = [ (zonedTimeToUTC t, r) | (t,r) <- readsPrec n s ]
