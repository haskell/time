-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.UTC
import Data.Time.Calendar.Days
import Data.Fixed
import Control.Monad

#include "HsTimeConfig.h"

#ifdef mingw32_HOST_OS
import Data.Word    ( Word64)
import System.Win32.Time
#elif HAVE_CLOCK_GETTIME
import Data.Time.Clock.CTimespec
import Foreign.C.Types (CTime(..))
#else
import Data.Time.Clock.CTimeval
#endif

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

-- | Get the current POSIX time from the system clock.
getPOSIXTime :: IO POSIXTime

#ifdef mingw32_HOST_OS
-- On Windows, the equlvalent of POSIX time is "file time", defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.

getPOSIXTime = do
  FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
  return (fromIntegral (ft - win32_epoch_adjust) / 10000000)

win32_epoch_adjust :: Word64
win32_epoch_adjust = 116444736000000000

#elif HAVE_CLOCK_GETTIME

-- Use hi-res POSIX time
ctimespecToPosixSeconds :: CTimespec -> POSIXTime
ctimespecToPosixSeconds (MkCTimespec (CTime s) ns) =
    (fromIntegral s) + (fromIntegral ns) / 1000000000

getPOSIXTime = liftM ctimespecToPosixSeconds getCTimespec

#else

-- Use POSIX time
ctimevalToPosixSeconds :: CTimeval -> POSIXTime
ctimevalToPosixSeconds (MkCTimeval s mus) = (fromIntegral s) + (fromIntegral mus) / 1000000

getPOSIXTime = liftM ctimevalToPosixSeconds getCTimeval

#endif

-- | Get the current UTC time from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = liftM posixSecondsToUTCTime getPOSIXTime
