{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Days (
    -- * Days
    Day (..),
    addDays,
    diffDays,
    HasDays (..),
    allDaysOf,
    dayCountOf,
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
class HasDays t where
    -- | Returns the first 'Day' in a period of days.
    firstDayOf :: t -> Day

    -- | Returns the last 'Day' in a period of days.
    lastDayOf :: t -> Day

-- | A list of all the days in this period
allDaysOf :: HasDays t => t -> [Day]
allDaysOf x = [firstDayOf x .. lastDayOf x]

-- | The number of days in this period.
dayCountOf :: HasDays t => t -> Integer
dayCountOf x = succ $ diffDays (lastDayOf x) (firstDayOf x)

instance HasDays Day where
    firstDayOf = id
    lastDayOf = id
