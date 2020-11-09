{-# LANGUAGE Safe #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Format.Parse
    (
    -- * UNIX-style parsing
      parseTimeM
    , parseTimeMultipleM
    , parseTimeOrError
    , readSTime
    , readPTime
    , ParseTime()
    -- * Locale
    , module Data.Time.Format.Locale
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.Char
import Data.Proxy
import Data.Traversable
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Format.Locale
import Data.Time.Format.Parse.Class
import Data.Time.Format.Parse.Instances ()
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.ZonedTime
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP hiding (char, string)

-- | Parses a time value given a format string.
-- Missing information will be derived from 1970-01-01 00:00 UTC (which was a Thursday).
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers, however padding widths are not supported.
-- Case is not significant in the input string.
-- Some variations in the input are accepted:
--
-- [@%z@ @%Ez@] accepts any of @±HHMM@ or @±HH:MM@.
--
-- [@%Z@ @%EZ@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
-- For example, to parse a date in YYYY-MM-DD format, while allowing the month
-- and date to have optional leading zeros (notice the @-@ modifier used for @%m@
-- and @%d@):
--
-- > Prelude Data.Time> parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe Day
-- > Just 2010-03-04
--
parseTimeM ::
       (MonadFail m, ParseTime t)
    => Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string.
    -> String -- ^ Input string.
    -> m t -- ^ Return the time value, or fail if the input could not be parsed using the given format.
parseTimeM acceptWS l fmt s = parseTimeMultipleM acceptWS l [(fmt, s)]

-- | Parses a time value given a list of pairs of format and input.
-- Resulting value is constructed from all provided specifiers.
parseTimeMultipleM' ::
       (MonadFail m, ParseTime t)
    => Proxy t
    -> Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> [(String, String)] -- ^ Pairs of (format string, input string).
    -> m t -- ^ Return the time value, or fail if the input could not be parsed using the given format.
parseTimeMultipleM' pt acceptWS l fmts = do
    specss <- for fmts $ \(fmt,s) -> parseTimeSpecifiersM pt acceptWS l fmt s
    case buildTime l $ mconcat specss of
        Just t -> return t
        Nothing -> fail "parseTimeM: cannot construct"

-- | Parses a time value given a list of pairs of format and input.
-- Resulting value is constructed from all provided specifiers.
parseTimeMultipleM ::
       (MonadFail m, ParseTime t)
    => Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> [(String, String)] -- ^ Pairs of (format string, input string).
    -> m t -- ^ Return the time value, or fail if the input could not be parsed using the given format.
parseTimeMultipleM = parseTimeMultipleM' Proxy

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTimeM' for details.
parseTimeOrError ::
       ParseTime t
    => Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string.
    -> String -- ^ Input string.
    -> t -- ^ The time value.
parseTimeOrError acceptWS l fmt s =
    case parseTimeM acceptWS l fmt s of
        [t] -> t
        [] -> error $ "parseTimeOrError: no parse of " ++ show s
        _ -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeSpecifiersM ::
       (MonadFail m, ParseTime t)
    => Proxy t
    -> Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> String -- ^ Input string.
    -> m [(Char, String)]
parseTimeSpecifiersM pt acceptWS l fmt s =
    case parseTimeSpecifiers pt acceptWS l fmt s of
        [t] -> return t
        [] -> fail $ "parseTimeM: no parse of " ++ show s
        _ -> fail $ "parseTimeM: multiple parses of " ++ show s

parseTimeSpecifiers ::
       ParseTime t
    => Proxy t
    -> Bool -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> String -- ^ Input string.
    -> [[(Char, String)]]
parseTimeSpecifiers pt False l fmt s = [t | (t, "") <- readP_to_S (readPSpecifiers pt False l fmt) s]
parseTimeSpecifiers pt True l fmt s = [t | (t, r) <- readP_to_S (readPSpecifiers pt True l fmt) s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readSTime ::
       ParseTime t
    => Bool -- ^ Accept leading whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> ReadS t
readSTime acceptWS l f = readP_to_S $ readPTime acceptWS l f

readPSpecifiers ::
       ParseTime t
    => Proxy t
    -> Bool -- ^ Accept leading whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> ReadP [(Char, String)]
readPSpecifiers pt False l f = parseSpecifiers pt l f
readPSpecifiers pt True l f = (skipSpaces >> parseSpecifiers pt l f) <++ parseSpecifiers pt l f

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime' ::
       ParseTime t
    => Proxy t
    -> Bool -- ^ Accept leading whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> ReadP t
readPTime' pt ws l f = do
    pairs <- readPSpecifiers pt ws l f
    case buildTime l pairs of
        Just t -> return t
        Nothing -> pfail

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime ::
       ParseTime t
    => Bool -- ^ Accept leading whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String -- ^ Format string
    -> ReadP t
readPTime = readPTime' Proxy

-- * Read instances for time package types
instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

-- | This only works for @±HHMM@ format,
-- single-letter military time-zones,
-- and these time-zones: \"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".
instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

-- | This only works for a 'zonedTimeZone' in @±HHMM@ format,
-- single-letter military time-zones,
-- and these time-zones: \"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".
instance Read ZonedTime where
    readsPrec n = readParen False $ \s -> [(ZonedTime t z, r2) | (t, r1) <- readsPrec n s, (z, r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = do
        (lt, s') <- readsPrec n s
        (tz, s'') <- readsPrec n s' <|> pure (utc, s')
        return (localTimeToUTC tz lt, s'')

instance Read UniversalTime where
    readsPrec n s = [(localTimeToUT1 0 t, r) | (t, r) <- readsPrec n s]
