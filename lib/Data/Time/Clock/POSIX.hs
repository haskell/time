-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime(..),posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.UTC
import Data.Time.Clock.Scale (picosecondsToDiffTime)
import Data.Time.Calendar.Days
import Data.Int (Int64)
import Data.Fixed (divMod')
import Control.DeepSeq

#include "HsTimeConfig.h"

#ifdef mingw32_HOST_OS
import Data.Word (Word64)
import System.Win32.Time
#elif HAVE_CLOCK_GETTIME
import Data.Time.Clock.CTimespec
import Foreign.C.Types (CTime(..), CLong(..))
#else
import Data.Time.Clock.CTimeval
import Foreign.C.Types (CLong(..))
#endif

--------------------------------------------------------------------------------

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
--
data POSIXTime = POSIXTime
    { ptSeconds ::     {-# UNPACK #-} !Int64
    , ptNanoSeconds :: {-# UNPACK #-} !Int64
    }

normalizePosix :: POSIXTime -> POSIXTime
normalizePosix raw@(POSIXTime xs xn)
    | xn < 0 || xn >= 1000000000 = POSIXTime (xs + q)  r
    | otherwise                  = raw
  where (q, r) = xn `divMod` 1000000000

instance Eq POSIXTime where
    rawx == rawy =
        let POSIXTime xs xn = normalizePosix rawx
            POSIXTime ys yn = normalizePosix rawy
        in xs == ys && xn == yn

instance Ord POSIXTime where
    rawx `compare` rawy =
        let POSIXTime xs xn = normalizePosix rawx
            POSIXTime ys yn = normalizePosix rawy
            os = compare xs ys
        in if os == EQ then xn `compare` yn else os

instance NFData POSIXTime where
    rnf a = a `seq` ()

posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime raw =
    let POSIXTime s ns = normalizePosix raw
        (d, s') = s `divMod` posixDayLength
        ps = s' * 1000000000000 + ns * 1000 -- 'Int64' can hold ps in one day
    in UTCTime (addDays (fromIntegral d) unixEpochDay)
               (picosecondsToDiffTime (fromIntegral ps))

posixDayLength :: Int64
posixDayLength = 86400

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

getPOSIXTime :: IO POSIXTime
#ifdef mingw32_HOST_OS
-- On Windows, the equlvalent of POSIX time is "file time", defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.

getPOSIXTime = do
    FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
    let (s, us) = (ft - win32_epoch_adjust) `divMod` 10000000
    return (POSIXTime (fromIntegral s) (fromIntegral us * 1000))
  where
    win32_epoch_adjust :: Word64
    win32_epoch_adjust = 116444736000000000

#elif HAVE_CLOCK_GETTIME
-- Use hi-res clock_gettime

getPOSIXTime = do
    MkCTimespec (CTime s) (CLong ns) <- getCTimespec
    return (POSIXTime (fromIntegral s) (fromIntegral ns))

#else
-- Use gettimeofday
getPOSIXTime = do
    MkCTimeval (CLong s) (CLong us) <- getCTimeval
    return (POSIXTime (fromIntegral s) (fromIntegral us * 1000))

#endif

--------------------------------------------------------------------------------

posixDayLength_ :: NominalDiffTime
posixDayLength_ = 86400

posixSecondsToUTCTime :: NominalDiffTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i posixDayLength_
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> NominalDiffTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * posixDayLength_) + min posixDayLength_ (realToFrac t)

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = posixToUTCTime `fmap` getPOSIXTime
