-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    POSIXTime,
    makePOSIXTime,ptSeconds,ptNanoSeconds,
    posixSecondsToUTCTime,utcTimeToPOSIXSeconds,
    posixToUTCTime,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.UTC
import Data.Time.Clock.Scale (picosecondsToDiffTime)
import Data.Time.Calendar.Days
import Data.Int (Int64)
import Data.Word
import Data.Fixed (divMod')
import Control.DeepSeq

#include "HsTimeConfig.h"

#ifdef mingw32_HOST_OS
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
    , ptNanoSeconds :: {-# UNPACK #-} !Word32
    } deriving (Eq,Ord)

#if 0
-- workaround for time test stanza dependency on unix, which needs fromRational and toRational

instance Show POSIXTime where
    show = error "undefined POSIXTime function"

instance Num POSIXTime where
    (+) = error "undefined POSIXTime function"
    (-) = error "undefined POSIXTime function"
    (*) = error "undefined POSIXTime function"
    negate = error "undefined POSIXTime function"
    abs = error "undefined POSIXTime function"
    signum = error "undefined POSIXTime function"
    fromInteger = error "undefined POSIXTime function"

instance Real POSIXTime where
    toRational (POSIXTime xs xn) = toRational xs + (toRational xn) / 1000000000

instance Fractional POSIXTime where
    fromRational r = makePOSIXTime 0 $ floor $ r * 1000000000
    recip = error "undefined POSIXTime function"
    (/) = error "undefined POSIXTime function"
#endif

makePOSIXTime :: Int64 -> Word32 -> POSIXTime
makePOSIXTime xs xn
    | xn < 0 || xn >= 1000000000 = POSIXTime (xs + fromIntegral q)  r
    | otherwise                  = POSIXTime xs xn
  where (q, r) = xn `divMod` 1000000000

instance NFData POSIXTime where
    rnf a = a `seq` ()

posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime (POSIXTime s ns) = let
    (d, s') = s `divMod` 86400
    ps :: Int64
    ps = s' * 1000000000000 + (fromIntegral ns) * 1000
    in UTCTime (addDays (fromIntegral d) unixEpochDay) (picosecondsToDiffTime $ fromIntegral ps)

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

posixSecondsToUTCTime :: NominalDiffTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i nominalDay
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> NominalDiffTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * nominalDay) + min nominalDay (realToFrac t)

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = posixToUTCTime `fmap` getPOSIXTime
