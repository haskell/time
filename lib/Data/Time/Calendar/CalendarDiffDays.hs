module Data.Time.Calendar.CalendarDiffDays
    (
        -- * Calendar Duration
        module Data.Time.Calendar.CalendarDiffDays
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif

data CalendarDiffDays = CalendarDiffDays
    { calendarMonths :: Integer
    , calendarDays :: Integer
    } deriving Eq

#if MIN_VERSION_base(4,9,0)
-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

-- | Additive
instance Monoid CalendarDiffDays where
    mempty = CalendarDiffDays 0 0
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend (CalendarDiffDays m1 d1) (CalendarDiffDays m2 d2) = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

-- | Show in ISO 8601 "PyyYmmMddD" format.
instance Show CalendarDiffDays where
    show dur@(CalendarDiffDays m d) = let
        (y,my) = quotRem m 12
        ys = if y == 0 then "" else show y ++ "Y"
        ms = if my == 0 then "" else show my ++ "M"
        ds = if d == 0 then "" else show d ++ "D"
        in if dur == mempty then "P0D" else "P" ++ ys ++ ms ++ ds

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)
