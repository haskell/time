{-# LANGUAGE Safe #-}

-- | TAI and leap-second maps for converting to UTC: most people won't need this module.
module Data.Time.Clock.Internal.AbsoluteTime (
    -- TAI arithmetic
    AbsoluteTime,
    taiEpoch,
    addAbsoluteTime,
    diffAbsoluteTime,
    taiNominalDayStart,
) where

import Control.DeepSeq
import Data.Data
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.DiffTime
import Language.Haskell.TH.Syntax qualified as TH

-- | AbsoluteTime is TAI, time as measured by a clock.
newtype AbsoluteTime
    = MkAbsoluteTime DiffTime
    deriving (Eq, Ord, Typeable, Data, TH.Lift)

instance NFData AbsoluteTime where
    rnf (MkAbsoluteTime a) = rnf a

-- | The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
taiEpoch :: AbsoluteTime
taiEpoch = MkAbsoluteTime 0

taiNominalDayStart :: Day -> AbsoluteTime
taiNominalDayStart day = MkAbsoluteTime $ realToFrac $ (toModifiedJulianDay day) * 86400

-- | addAbsoluteTime a b = a + b
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

-- | diffAbsoluteTime a b = a - b
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b
