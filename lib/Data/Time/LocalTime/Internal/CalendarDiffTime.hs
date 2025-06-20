{-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.CalendarDiffTime (
    -- * Calendar Duration
    module Data.Time.LocalTime.Internal.CalendarDiffTime,
) where

import Control.DeepSeq
import Data.Data
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Clock.Internal.NominalDiffTime
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH

data CalendarDiffTime = CalendarDiffTime
    { ctMonths :: Integer
    , ctTime :: NominalDiffTime
    }
    deriving (Eq, Typeable, Data, Generic, TH.Lift)

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime m t) = rnf m `seq` rnf t `seq` ()

-- | Additive
instance Semigroup CalendarDiffTime where
    CalendarDiffTime m1 d1 <> CalendarDiffTime m2 d2 = CalendarDiffTime (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffTime where
    mempty = CalendarDiffTime 0 0
    mappend = (<>)

calendarTimeDays :: CalendarDiffDays -> CalendarDiffTime
calendarTimeDays (CalendarDiffDays m d) = CalendarDiffTime m $ fromInteger d * nominalDay

calendarTimeTime :: NominalDiffTime -> CalendarDiffTime
calendarTimeTime dt = CalendarDiffTime 0 dt

-- | Scale by a factor. Note that @scaleCalendarDiffTime (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffTime :: Integer -> CalendarDiffTime -> CalendarDiffTime
scaleCalendarDiffTime k (CalendarDiffTime m d) = CalendarDiffTime (k * m) (fromInteger k * d)
