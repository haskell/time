-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.GetTime
import Data.Time.Clock.UTC
import Data.Time.Clock.Raw
import Data.Time.Calendar.Days
import Data.Fixed

-- | 86400 nominal seconds in every day
posixDayLength :: NominalDiffTime
posixDayLength = 86400

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
--
-- To convert from a 'Foreign.C.CTime' or 'System.Posix.EpochTime', use 'realToFrac'.
--
type POSIXTime = NominalDiffTime

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i posixDayLength
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * posixDayLength) + min posixDayLength (realToFrac t)

rawTimeToPOSIXTime :: RawTime -> POSIXTime
rawTimeToPOSIXTime (MkRawTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9

-- | Get the current POSIX time from the system clock.
getPOSIXTime :: IO POSIXTime
getPOSIXTime = fmap rawTimeToPOSIXTime getRawTime
