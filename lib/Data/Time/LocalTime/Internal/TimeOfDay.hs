{-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.TimeOfDay (
    -- * Time of day
    TimeOfDay (..),
    midnight,
    midday,
    makeTimeOfDayValid,
    timeToDaysAndTimeOfDay,
    daysAndTimeOfDayToTime,
    utcToLocalTimeOfDay,
    localToUTCTimeOfDay,
    timeToTimeOfDay,
    pastMidnight,
    timeOfDayToTime,
    sinceMidnight,
    dayFractionToTimeOfDay,
    timeOfDayToDayFraction,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Time.Calendar.Private
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.LocalTime.Internal.TimeZone
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH

-- | Time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day.
--
-- @TimeOfDay 24 0 0@ is considered invalid for the purposes of 'makeTimeOfDayValid', as well as reading and parsing,
-- but valid for ISO 8601 parsing in "Data.Time.Format.ISO8601".
data TimeOfDay = TimeOfDay
    { todHour :: Int
    -- ^ range 0 - 23
    , todMin :: Int
    -- ^ range 0 - 59
    , todSec :: Pico
    -- ^ Note that 0 <= 'todSec' < 61, accomodating leap seconds.
    -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
    }
    deriving (Eq, Ord, Typeable, Data, Generic, TH.Lift)

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` rnf s `seq` ()

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
    show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

makeTimeOfDayValid :: Int -> Int -> Pico -> Maybe TimeOfDay
makeTimeOfDayValid h m s = do
    _ <- clipValid 0 23 h
    _ <- clipValid 0 59 m
    _ <- clipValid 0 60.999999999999 s
    return (TimeOfDay h m s)

-- | Convert a period of time into a count of days and a time of day since midnight.
-- The time of day will never have a leap second.
timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer, TimeOfDay)
timeToDaysAndTimeOfDay dt =
    let
        s = realToFrac dt
        (m, ms) = divMod' s 60
        (h, hm) = divMod' m 60
        (d, dh) = divMod' h 24
    in
        (d, TimeOfDay dh hm ms)

-- | Convert a count of days and a time of day since midnight into a period of time.
daysAndTimeOfDayToTime :: Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (TimeOfDay dh hm ms) =
    (+) (realToFrac ms) $ (*) 60 $ (+) (realToFrac hm) $ (*) 60 $ (+) (realToFrac dh) $ (*) 24 $ realToFrac d

-- | Convert a time of day in UTC to a time of day in some timezone, together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24), TimeOfDay (mod h' 24) (mod m' 60) s)
  where
    m' = m + timeZoneMinutes zone
    h' = h + (div m' 60)

-- | Convert a time of day in some timezone to a time of day in UTC, together with a day adjustment.
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

-- | Get the time of day given a time since midnight.
-- Time more than 24h will be converted to leap-seconds.
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt
    | dt >= posixDayLength = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDayLength)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s
  where
    s' = realToFrac dt
    s = mod' s' 60
    m' = div' s' 60
    m = mod' m' 60
    h = div' m' 60

-- | Same as 'timeToTimeOfDay'.
pastMidnight :: DiffTime -> TimeOfDay
pastMidnight = timeToTimeOfDay

-- | Get the time since midnight for a given time of day.
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | Same as 'timeOfDayToTime'.
sinceMidnight :: TimeOfDay -> DiffTime
sinceMidnight = timeOfDayToTime

-- | Get the time of day given the fraction of a day since midnight.
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | Get the fraction of a day since midnight given a time of day.
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod) / realToFrac posixDayLength
