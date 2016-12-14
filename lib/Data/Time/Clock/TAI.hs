{-# OPTIONS -fno-warn-unused-imports #-}
#include "HsConfigure.h"
-- | TAI and leap-second maps for converting to UTC: most people won't need this module.
module Data.Time.Clock.TAI
(
    -- TAI arithmetic
    AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,

    -- leap-second map type
    LeapSecondMap,

    -- conversion between UTC and TAI with map
    utcDayLength,utcToTAITime,taiToUTCTime,

    taiClock,
) where

import Data.Time.LocalTime
import Data.Time.Calendar.Days
import Data.Time.Clock.GetTime
import Data.Time.Clock
import Control.DeepSeq
import Data.Maybe
import Data.Typeable
import Data.Fixed
#if LANGUAGE_Rank2Types
import Data.Data
#endif

-- | AbsoluteTime is TAI, time as measured by a clock.
newtype AbsoluteTime = MkAbsoluteTime DiffTime deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    ,Data, Typeable
#endif
#endif
#endif
    )

instance NFData AbsoluteTime where
    rnf (MkAbsoluteTime a) = rnf a

instance Show AbsoluteTime where
    show t = show (utcToLocalTime utc (fromJust (taiToUTCTime (const (Just 0)) t))) ++ " TAI" -- ugly, but standard apparently

-- | The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
taiEpoch :: AbsoluteTime
taiEpoch = MkAbsoluteTime 0

-- | addAbsoluteTime a b = a + b
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

-- | diffAbsoluteTime a b = a - b
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b

-- | TAI - UTC during this day.
-- No table is provided, as any program compiled with it would become
-- out of date in six months.
type LeapSecondMap = Day -> Maybe Int

utcDayLength :: LeapSecondMap -> Day -> Maybe DiffTime
utcDayLength lsmap day = do
    i0 <- lsmap day
    i1 <- lsmap $ addDays 1 day
    return $ realToFrac (86400 + i1 - i0)

dayStart :: LeapSecondMap -> Day -> Maybe AbsoluteTime
dayStart lsmap day = do
    i <- lsmap day
    return $ addAbsoluteTime (realToFrac $ (toModifiedJulianDay day) * 86400 + toInteger i) taiEpoch

utcToTAITime :: LeapSecondMap -> UTCTime -> Maybe AbsoluteTime
utcToTAITime lsmap (UTCTime day dtime) = do
    t <- dayStart lsmap day
    return $ addAbsoluteTime dtime t

taiToUTCTime :: LeapSecondMap -> AbsoluteTime -> Maybe UTCTime
taiToUTCTime lsmap abstime = let
    stable day = do
        dayt <- dayStart lsmap day
        len <- utcDayLength lsmap day
        let
            dtime = diffAbsoluteTime abstime dayt
            day' = addDays (div' dtime len) day
        if day == day' then return (UTCTime day dtime) else stable day'
    in stable $ ModifiedJulianDay $ div' (diffAbsoluteTime abstime taiEpoch) 86400

rawToTAITime :: POSIXTime -> AbsoluteTime
rawToTAITime (POSIXTime s ns) = MkAbsoluteTime $ (fromIntegral s) + (fromIntegral ns) * 1E-9

taiClock :: Maybe (DiffTime,IO AbsoluteTime)
taiClock = fmap (fmap (fmap rawToTAITime)) getTAIRawTime
