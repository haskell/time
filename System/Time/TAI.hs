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

-- | TAI
type AbsoluteTime = MkAbsoluteTime Integer

addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime

diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime

-- | TAI - UTC during this day
type LeapSecondTable = ModJulianDay -> Int

utcDayLength :: LeapSecondTable -> ModJulianDay -> DiffTime
utcDayLength table day = siSecondsToTime (86400 + (table (day + 1)) - (table day))

utcToTAITime :: LeapSecondTable -> UTCTime -> TAITime
utcToTAITime table (UTCTime day dtime) = siSecondsToTime (table day) + 

taiToUTCTime :: LeapSecondTable -> TAITime -> UTCTime

