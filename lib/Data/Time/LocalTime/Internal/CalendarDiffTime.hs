{-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.CalendarDiffTime
    (
        -- * Calendar Duration
        module Data.Time.LocalTime.Internal.CalendarDiffTime
    ) where
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup hiding (option)
#endif
import Data.Fixed
import Data.Typeable
import Data.Data
import Control.DeepSeq
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Clock.Internal.NominalDiffTime

data CalendarDiffTime = CalendarDiffTime
    { ctMonths :: Integer
    , ctTime :: NominalDiffTime
    } deriving (Eq,
    Data
#if __GLASGOW_HASKELL__ >= 802
    -- ^ @since 1.9.2
#endif
    ,Typeable
#if __GLASGOW_HASKELL__ >= 802
    -- ^ @since 1.9.2
#endif
    )

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime m t) = rnf m `seq` rnf t `seq` ()

-- | Additive
instance Semigroup CalendarDiffTime where
    CalendarDiffTime m1 d1 <> CalendarDiffTime m2 d2 = CalendarDiffTime (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffTime where
    mempty = CalendarDiffTime 0 0
    mappend = (<>)

instance Show CalendarDiffTime where
    show (CalendarDiffTime m t) = "P" ++ show m ++ "MT" ++ showFixed True (realToFrac t :: Pico) ++ "S"

calendarTimeDays :: CalendarDiffDays -> CalendarDiffTime
calendarTimeDays (CalendarDiffDays m d) = CalendarDiffTime m $ fromInteger d * nominalDay

calendarTimeTime :: NominalDiffTime -> CalendarDiffTime
calendarTimeTime dt = CalendarDiffTime 0 dt

-- | Scale by a factor. Note that @scaleCalendarDiffTime (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffTime :: Integer -> CalendarDiffTime -> CalendarDiffTime
scaleCalendarDiffTime k (CalendarDiffTime m d) = CalendarDiffTime (k * m) (fromInteger k * d)
