{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.TimeOfDay
(
	TimeOfDay(..),midnight,midday,
	utcToLocalTimeOfDay,localToUTCTimeOfDay,
	timeToTimeOfDay,timeOfDayToTime,
	dayFractionToTimeOfDay,timeOfDayToDayFraction
) where

import System.Time.Clock
import System.Time.Calendar.Timezone
import Data.Fixed

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

show2Fixed :: Pico -> String
show2Fixed x | x < 10 = '0':(showFixed True x)
show2Fixed x = showFixed True x

instance Show TimeOfDay where
	show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

-- | convert a ToD in UTC to a ToD in some timezone, together with a day adjustment
utcToLocalTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
	m' = m + timezoneToMinutes zone
	h' = h + (div m' 60)

-- | convert a ToD in some timezone to a ToD in UTC, together with a day adjustment
localToUTCTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimezone (negate (timezoneToMinutes zone)))

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
