-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
    ( -- * Raw POSIX timestamps
      POSIXTimeRaw(..)
    , normalizeRaw
    , posixRawToPOSIX
    , posixRawToUTCTime
    , getPOSIXTimeRaw
    , posixDayLengthRaw
     -- * high precision POSIX time type
    , posixDayLength
    , POSIXTime
    , unixEpochDay
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    , getPOSIXTime
    , getCurrentTime
    ) where

import Data.Time.Clock.UTC
import Data.Time.Clock.Scale (picosecondsToDiffTime)
import Data.Time.Calendar.Days
import Data.Fixed
import Control.Monad
import Data.Int (Int64)

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

data POSIXTimeRaw = POSIXTimeRaw
    { ptSeconds ::     {-# UNPACK #-} !Int64
    , ptNanoSeconds :: {-# UNPACK #-} !Int64
    }

normalizeRaw :: POSIXTimeRaw -> POSIXTimeRaw
normalizeRaw (POSIXTimeRaw xs xn)
    | xn < 0 || xn >= 1000000000 = POSIXTimeRaw (xs + q)  r
    | otherwise                  = POSIXTimeRaw xs xn
  where (q, r) = xn `divMod` 1000000000

instance Eq POSIXTimeRaw where
    rawx == rawy =
        let POSIXTimeRaw xs xn = normalizeRaw rawx
            POSIXTimeRaw ys yn = normalizeRaw rawy
        in xs == ys && xn == yn

instance Ord POSIXTimeRaw where
    rawx `compare` rawy =
        let POSIXTimeRaw xs xn = normalizeRaw rawx
            POSIXTimeRaw ys yn = normalizeRaw rawy
            os = compare xs ys
        in if os == EQ then xn `compare` yn else os

posixRawToPOSIX :: POSIXTimeRaw -> POSIXTime
posixRawToPOSIX (POSIXTimeRaw s ns) =
    let ps = fromIntegral s * 1000000000000 + fromIntegral ns * 1000
    in realToFrac (picosecondsToDiffTime ps)

posixRawToUTCTime :: POSIXTimeRaw -> UTCTime
posixRawToUTCTime raw =
    let POSIXTimeRaw s ns = normalizeRaw raw
        (d, s') = s `divMod` posixDayLengthRaw
        ps = s' * 1000000000000 + ns * 1000 -- 'Int64' can hold ps in one day
    in UTCTime (addDays (fromIntegral d) unixEpochDay)
               (picosecondsToDiffTime (fromIntegral ps))

posixDayLengthRaw :: Int64
posixDayLengthRaw = 86400

getPOSIXTimeRaw :: IO POSIXTimeRaw
#ifdef mingw32_HOST_OS
-- On Windows, the equlvalent of POSIX time is "file time", defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.

getPOSIXTimeRaw = do
    FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
    let (s, us) = (ft - win32_epoch_adjust) `divMod` 10000000
    return (POSIXTimeRaw (fromIntegral s) (fromIntegral us * 1000))
  where
    win32_epoch_adjust :: Word64
    win32_epoch_adjust = 116444736000000000

#elif HAVE_CLOCK_GETTIME
-- Use hi-res clock_gettime

getPOSIXTimeRaw = do
    MkCTimespec (CTime s) (CLong ns) <- getCTimespec
    return (POSIXTimeRaw s ns)

#else
-- Use gettimeofday
getPOSIXTimeRaw = do
    MkCTimeval (CLong s) (CLong us) <- getCTimeval
    return (POSIXTimeRaw s (us * 1000))

#endif

--------------------------------------------------------------------------------

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

-- | Get the current 'POSIXTime' from the system clock.
getPOSIXTime :: IO POSIXTime
getPOSIXTime = posixRawToPOSIX `fmap` getPOSIXTimeRaw

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = posixRawToUTCTime `fmap` getPOSIXTimeRaw
