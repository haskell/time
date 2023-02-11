{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveLift #-}

module Data.Time.Calendar.CalendarDiffDays (
    -- * Calendar Duration
    module Data.Time.Calendar.CalendarDiffDays,
) where

import Control.DeepSeq
import Data.Data
import qualified Language.Haskell.TH.Syntax as TH

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    }
    deriving
        ( Eq
        , -- | @since 1.9.2
          Data
        , -- | @since 1.9.2
          Typeable
          -- | @since 1.13.0
        , TH.Lift
        )

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays m d) = rnf m `seq` rnf d `seq` ()

-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffDays where
    mempty = CalendarDiffDays 0 0
    mappend = (<>)

instance Show CalendarDiffDays where
    show (CalendarDiffDays m d) = "P" ++ show m ++ "M" ++ show d ++ "D"

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
