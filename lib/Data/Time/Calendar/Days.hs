{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Days (
    -- * Days
    Day (..),
    addDays,
    diffDays,

    -- * DayPeriod
    DayPeriod (..),
    periodAllDays,
    periodLength,
    periodFromDay,
    periodToDay,
    periodToDayValid,
) where

import Control.DeepSeq
import Data.Data
import Data.Ix

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Integer
    }
    deriving (Eq, Ord, Data, Typeable)

instance NFData Day where
    rnf (ModifiedJulianDay a) = rnf a

instance Enum Day where
    succ (ModifiedJulianDay a) = ModifiedJulianDay (succ a)
    pred (ModifiedJulianDay a) = ModifiedJulianDay (pred a)
    toEnum = ModifiedJulianDay . toEnum
    fromEnum (ModifiedJulianDay a) = fromEnum a
    enumFrom (ModifiedJulianDay a) = fmap ModifiedJulianDay (enumFrom a)
    enumFromThen (ModifiedJulianDay a) (ModifiedJulianDay b) = fmap ModifiedJulianDay (enumFromThen a b)
    enumFromTo (ModifiedJulianDay a) (ModifiedJulianDay b) = fmap ModifiedJulianDay (enumFromTo a b)
    enumFromThenTo (ModifiedJulianDay a) (ModifiedJulianDay b) (ModifiedJulianDay c) =
        fmap ModifiedJulianDay (enumFromThenTo a b c)

instance Ix Day where
    range (ModifiedJulianDay a, ModifiedJulianDay b) = fmap ModifiedJulianDay (range (a, b))
    index (ModifiedJulianDay a, ModifiedJulianDay b) (ModifiedJulianDay c) = index (a, b) c
    inRange (ModifiedJulianDay a, ModifiedJulianDay b) (ModifiedJulianDay c) = inRange (a, b) c
    rangeSize (ModifiedJulianDay a, ModifiedJulianDay b) = rangeSize (a, b)

addDays :: Integer -> Day -> Day
addDays n (ModifiedJulianDay a) = ModifiedJulianDay (a + n)

diffDays :: Day -> Day -> Integer
diffDays (ModifiedJulianDay a) (ModifiedJulianDay b) = a - b

-- | The class of types which can be represented as a period of days.
class Ord p => DayPeriod p where
    -- | Returns the first 'Day' in a period of days.
    periodFirstDay :: p -> Day

    -- | Returns the last 'Day' in a period of days.
    periodLastDay :: p -> Day

    -- | Get the period this day is in
    dayPeriod :: Day -> p

-- | A list of all the days in this period
periodAllDays :: DayPeriod p => p -> [Day]
periodAllDays x = [periodFirstDay x .. periodLastDay x]

-- | The number of days in this period.
periodLength :: DayPeriod p => p -> Integer
periodLength x = succ $ diffDays (periodLastDay x) (periodFirstDay x)

-- | Get the period this day is in, with the 1-based day number within the period.
-- `periodFromDay (periodFirstDay x) = (x,1)`
periodFromDay :: DayPeriod p => Day -> (p, Int)
periodFromDay d =
    let p = dayPeriod d
        dt = succ $ diffDays d (periodFirstDay p)
     in (p, fromInteger dt)

-- | Inverse of `periodFromDay`
periodToDay :: DayPeriod p => p -> Int -> Day
periodToDay p i = addDays (toInteger $ pred i) $ periodFirstDay p

-- | Validating inverse of `periodFromDay`
periodToDayValid :: DayPeriod p => p -> Int -> Maybe Day
periodToDayValid p i =
    let d = periodToDay p i
     in if fst (periodFromDay d) == p then Just d else Nothing

instance DayPeriod Day where
    periodFirstDay = id
    periodLastDay = id
    dayPeriod = id
