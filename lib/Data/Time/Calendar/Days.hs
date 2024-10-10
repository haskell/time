{-# LANGUAGE CPP #-}
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
#ifdef __GLASGOW_HASKELL__
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Integer
    }
    deriving (Eq, Ord, Data, Typeable
#ifdef __GLASGOW_HASKELL__
             , TH.Lift, Generic
#endif
             )

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
--
-- @since 1.12.1
class Ord p => DayPeriod p where
    -- | Returns the first 'Day' in a period of days.
    periodFirstDay :: p -> Day

    -- | Returns the last 'Day' in a period of days.
    periodLastDay :: p -> Day

    -- | Get the period this day is in.
    dayPeriod :: Day -> p

-- | A list of all the days in this period.
--
-- @since 1.12.1
periodAllDays :: DayPeriod p => p -> [Day]
periodAllDays p = [periodFirstDay p .. periodLastDay p]

-- | The number of days in this period.
--
-- @since 1.12.1
periodLength :: DayPeriod p => p -> Int
periodLength p = succ $ fromInteger $ diffDays (periodLastDay p) (periodFirstDay p)

-- | Get the period this day is in, with the 1-based day number within the period.
--
-- @periodFromDay (periodFirstDay p) = (p,1)@
--
-- @since 1.12.1
periodFromDay :: DayPeriod p => Day -> (p, Int)
periodFromDay d =
    let
        p = dayPeriod d
        dt = succ $ fromInteger $ diffDays d $ periodFirstDay p
    in
        (p, dt)

-- | Inverse of 'periodFromDay'.
--
-- @since 1.12.1
periodToDay :: DayPeriod p => p -> Int -> Day
periodToDay p i = addDays (toInteger $ pred i) $ periodFirstDay p

-- | Validating inverse of 'periodFromDay'.
--
-- @since 1.12.1
periodToDayValid :: DayPeriod p => p -> Int -> Maybe Day
periodToDayValid p i =
    let
        d = periodToDay p i
    in
        if fst (periodFromDay d) == p then Just d else Nothing

instance DayPeriod Day where
    periodFirstDay = id
    periodLastDay = id
    dayPeriod = id
