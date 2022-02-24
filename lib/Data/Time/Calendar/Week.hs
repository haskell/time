{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Week (
    -- * Week
    DayOfWeek (..),
    dayOfWeek,
    dayOfWeekDiff,
    firstDayOfWeekOnAfter,
    weekAllDays,
    weekFirstDay,
    weekLastDay,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Ix
import Data.Time.Calendar.Days

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Read, Data, Typeable, Ord, Ix)

instance NFData DayOfWeek where
    rnf Monday = ()
    rnf Tuesday = ()
    rnf Wednesday = ()
    rnf Thursday = ()
    rnf Friday = ()
    rnf Saturday = ()
    rnf Sunday = ()

-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
instance Enum DayOfWeek where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday = 1
    fromEnum Tuesday = 2
    fromEnum Wednesday = 3
    fromEnum Thursday = 4
    fromEnum Friday = 5
    fromEnum Saturday = 6
    fromEnum Sunday = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d

-- | Returns a week containing the given 'Day' where the first day is the
-- 'DayOfWeek' specified.
--
-- Examples:
--
-- >>> weekAllDays Sunday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 20 .. YearMonthDay 2022 2 26]
--
-- >>> weekAllDays Monday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 21 .. YearMonthDay 2022 2 27]
--
-- >>> weekAllDays Tuesday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 15 .. YearMonthDay 2022 2 21]
--
-- @since 1.12.2
weekAllDays :: DayOfWeek -> Day -> [Day]
weekAllDays firstDay day = take 7 [weekFirstDay firstDay day ..]

-- | Returns the first day of a week containing the given 'Day'.
--
-- Examples:
--
-- >>> weekFirstDay Sunday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 20
--
-- >>> weekFirstDay Monday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 21
--
-- >>> weekFirstDay Tuesday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 15
--
-- @since 1.12.2
weekFirstDay :: DayOfWeek -> Day -> Day
weekFirstDay firstDay day = addDays (negate $ dayOfWeekDiffByDay firstDay day) day

-- | Returns the last day of a week containing the given 'Day'.
--
-- Examples:
--
-- >>> weekLastDay Sunday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 26
--
-- >>> weekLastDay Monday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 27
--
-- >>> weekLastDay Tuesday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 21
--
-- @since 1.12.2
weekLastDay :: DayOfWeek -> Day -> Day
weekLastDay firstDay day = addDays (6 - dayOfWeekDiffByDay firstDay day) day

dayOfWeekDiffByDay :: DayOfWeek -> Day -> Integer
dayOfWeekDiffByDay firstDay day = toInteger $ dayOfWeekDiff (dayOfWeek day) firstDay
