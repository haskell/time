{-# OPTIONS -fno-warn-unused-imports #-}
-- #hide
module Data.Time.LocalTime.TimeOfDay
(
    -- * Time of day
    TimeOfDay(..),midnight,midday,makeTimeOfDayValid,
    utcToLocalTimeOfDay,localToUTCTimeOfDay,
    timeToTimeOfDay,timeOfDayToTime,
    dayFractionToTimeOfDay,timeOfDayToDayFraction
) where

import Data.Time.LocalTime.TimeZone
import Data.Time.Calendar.Private
import Data.Time.Clock.Scale
import Data.Time.Clock.POSIX (posixDayLength)
import Control.DeepSeq
import Data.Typeable
import Data.Fixed
#if LANGUAGE_Rank2Types
import Data.Data
#endif

-- | Time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day.
data TimeOfDay = TimeOfDay {
    -- | range 0 - 23
    todHour    :: {-# UNPACK #-} !Int,
    -- | range 0 - 59
    todMin     :: {-# UNPACK #-} !Int,
    -- | Note that 0 <= todSec < 61, accomodating leap seconds.
    -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
    todSec     :: !Pico
} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
    ,Data, Typeable
#endif
#endif
    )

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = h `deepseq` m `deepseq` s `seq` () -- FIXME: Data.Fixed had no NFData instances yet at time of writing

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
    show (TimeOfDay h m s) = (show2 (Just '0') h) ++ ":" ++ (show2 (Just '0') m) ++ ":" ++ (show2Fixed (Just '0') s)

makeTimeOfDayValid :: Int -> Int -> Pico -> Maybe TimeOfDay
makeTimeOfDayValid h m s = do
    _ <- clipValid 0 23 h
    _ <- clipValid 0 59 m
    _ <- clipValid 0 60.999999999999 s
    return (TimeOfDay h m s)

-- | Convert a ToD in UTC to a ToD in some timezone, together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
    m' = m + timeZoneMinutes zone
    h' = h + (div m' 60)

-- | Convert a ToD in some timezone to a ToD in UTC, together with a day adjustment.
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayDiff :: DiffTime
posixDayDiff = fromIntegral posixDayLength

-- | Get a TimeOfDay given a time since midnight.
-- Time more than 24h will be converted to leap-seconds.
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDayDiff = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDayDiff)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s where
    s' = realToFrac dt
    s = mod' s' 60
    m' = div' s' 60
    m = mod' m' 60
    h = div' m' 60

-- | Find out how much time since midnight a given TimeOfDay is.
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | Get a TimeOfDay given the fraction of a day since midnight.
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | Get the fraction of a day since midnight given a TimeOfDay.
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod) / realToFrac posixDayDiff
