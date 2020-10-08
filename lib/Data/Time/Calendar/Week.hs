module Data.Time.Calendar.Week
    (
      -- * Week
      DayOfWeek(..)
    , dayOfWeek
    , dayOfWeekDiff
    , firstDayOfWeekOnAfter
    ) where

import Data.Fixed
import Data.Data
import Data.Time.Calendar.Days

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Read, Data, Typeable, Ord)

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
