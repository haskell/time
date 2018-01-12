module Data.Time.Calendar.Duration
    (
        -- * Calendar Duration
        module Data.Time.Calendar.Duration
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif

data CalendarDuration = CalendarDuration
    { calendarMonths :: Integer
    , calendarDays :: Integer
    } deriving Eq

#if MIN_VERSION_base(4,9,0)
-- | Additive
instance Semigroup CalendarDuration where
    CalendarDuration m1 d1 <> CalendarDuration m2 d2 = CalendarDuration (m1 + m2) (d1 + d2)
#endif

-- | Additive
instance Monoid CalendarDuration where
    mempty = CalendarDuration 0 0
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend (CalendarDuration m1 d1) (CalendarDuration m2 d2) = CalendarDuration (m1 + m2) (d1 + d2)
#endif

-- | Show in ISO 8601 "PyyYmmMddD" format.
instance Show CalendarDuration where
    show dur@(CalendarDuration m d) = let
        (y,my) = quotRem m 12
        ys = if y == 0 then "" else show y ++ "Y"
        ms = if my == 0 then "" else show my ++ "M"
        ds = if d == 0 then "" else show d ++ "D"
        in if dur == mempty then "P0D" else "P" ++ ys ++ ms ++ ds

calendarDay :: CalendarDuration
calendarDay = CalendarDuration 0 1

calendarWeek :: CalendarDuration
calendarWeek = CalendarDuration 0 7

calendarMonth :: CalendarDuration
calendarMonth = CalendarDuration 1 0

calendarYear :: CalendarDuration
calendarYear = CalendarDuration 12 0

-- | Scale by a factor. Note that @scaleCalendarDuration (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDuration :: Integer -> CalendarDuration -> CalendarDuration
scaleCalendarDuration k (CalendarDuration m d) = CalendarDuration (k * m) (k * d)
