{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.TimeOfDay
(
	TimeOfDay(..),midnight,midday,
	utcToLocalTimeOfDay,localToUTCTimeOfDay,
	timeToTimeOfDay,timeOfDayToTime,
	dayFractionToTimeOfDay,timeOfDayToDayFraction
) where

import System.Time.Calendar.Timezone
import System.Time.Calendar.Format
import System.Time.Calendar.Private
import System.Time.Clock
import Data.Fixed

import System.Locale
import Data.Char

-- | time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day
data TimeOfDay = TimeOfDay {
	todHour    :: Int,
	todMin     :: Int,
	todSec     :: Pico
} deriving (Eq,Ord)

midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
	show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

instance FormatTime TimeOfDay where
	formatCharacter 'H' = Just (\_ (TimeOfDay h _ _) -> show2 h)
	formatCharacter 'I' = Just (\_ (TimeOfDay h _ _) -> show2 ((mod (h - 1) 12) + 1))
	formatCharacter 'k' = Just (\_ (TimeOfDay h _ _) -> show2Space h)
	formatCharacter 'l' = Just (\_ (TimeOfDay h _ _) -> show2Space ((mod (h - 1) 12) + 1))
	formatCharacter 'M' = Just (\_ (TimeOfDay _ m _) -> show2 m)
	formatCharacter 'p' = Just (\locale (TimeOfDay h _ _) -> (if h < 12 then fst else snd) (amPm locale))
	formatCharacter 'P' = Just (\locale (TimeOfDay h _ _) -> map toLower ((if h < 12 then fst else snd) (amPm locale)))
	formatCharacter 'r' = Just (\locale time -> formatTime locale (time12Fmt locale) time)
	formatCharacter 'R' = Just (\locale time -> formatTime locale "%H:%M" time)
	formatCharacter 'S' = Just (\_ (TimeOfDay _ _ s) -> show2Fixed s)
	formatCharacter 'T' = Just (\locale time -> formatTime locale "%H:%M:%S" time)
	formatCharacter 'X' = Just (\locale time -> formatTime locale (timeFmt locale) time)
	formatCharacter _   = Nothing

-- | convert a ToD in UTC to a ToD in some timezone, together with a day adjustment
utcToLocalTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
	m' = m + timezoneMinutes zone
	h' = h + (div m' 60)

-- | convert a ToD in some timezone to a ToD in UTC, together with a day adjustment
localToUTCTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimezone (negate (timezoneMinutes zone)))

posixDay :: DiffTime
posixDay = fromInteger 86400

-- | get a TimeOfDay given a time since midnight
-- | time more than 24h will be converted to leap-seconds
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDay = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDay)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s where
	s' = realToFrac dt
	s = mod' s' 60
	m' = div' s' 60
	m = mod' m' 60
	h = div' m' 60

-- | find out how much time since midnight a given TimeOfDay is
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | get a TimeOfDay given the fraction of a day since midnight
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | get the fraction of a day since midnight given a TimeOfDay
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod / posixDay)
