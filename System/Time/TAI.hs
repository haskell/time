-- | most people won't need this module
module System.Time.TAI
(
	-- TAI arithmetic
	AbsoluteTime,addAbsoluteTime,diffAbsoluteTime,

	-- leap-second table type
	LeapSecondTable,

	-- conversion between UTC and TAI with table
	utcDayLength,utcToTAITime,taiToUTCTime
) where

import System.Time.Clock

-- | TAI as DiffTime from epoch
newtype AbsoluteTime = MkAbsoluteTime DiffTime deriving (Eq,Ord)

addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (t + a)

diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b

-- | TAI - UTC during this day
type LeapSecondTable = ModJulianDay -> Integer

utcDayLength :: LeapSecondTable -> ModJulianDay -> DiffTime
utcDayLength table day = siSecondsToTime (86400 + (table (day + 1)) - (table day))

utcToTAITime :: LeapSecondTable -> UTCTime -> AbsoluteTime
utcToTAITime table (UTCTime day dtime) = MkAbsoluteTime
	((siSecondsToTime (day * 86400 + (table day))) + dtime)

taiToUTCTime :: LeapSecondTable -> AbsoluteTime -> UTCTime
taiToUTCTime table (MkAbsoluteTime t) = undefined
