{-# LANGUAGE Trustworthy #-}
module Data.Time.Clock.GetTime where

import Data.Int (Int64)
import Data.Word
import Control.DeepSeq
import Data.Time.Clock.Scale

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

-- | 'RawTime' is time returned by system clock functions.
-- Its semantics depends on the clock function.
-- Note that 'rawNanoseconds' of 1E9 to 2E9-1 can be used to represent leap seconds.
data RawTime = MkRawTime
    { rawSeconds ::     {-# UNPACK #-} !Int64
    , rawNanoseconds :: {-# UNPACK #-} !Word32
    } deriving (Eq,Ord)

instance NFData RawTime where
    rnf a = a `seq` ()

-- | Get POSIX time, epoch start of 1970 UTC, leap-seconds ignored
getRawTime :: IO RawTime

-- | The resolution of 'getRawTime', 'getCurrentTime', 'getPOSIXTime'
getTime_resolution :: DiffTime

-- | If supported, get TAI time, epoch start of 1970 TAI, with resolution.
-- This is supported only on UNIX systems, and only those with CLOCK_TAI available at run-time.
getTAIRawTime :: Maybe (DiffTime,IO RawTime)

#ifdef mingw32_HOST_OS
-- On Windows, the equlvalent of POSIX time is "file time", defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.

getRawTime = do
    FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
    let (s, us) = (ft - win32_epoch_adjust) `divMod` 10000000
    return (MkRawTime (fromIntegral s) (fromIntegral us * 1000))
  where
    win32_epoch_adjust :: Word64
    win32_epoch_adjust = 116444736000000000
getTime_resolution = 1E-6 -- microsecond
getTAIRawTime = Nothing

#elif HAVE_CLOCK_GETTIME
-- Use hi-res clock_gettime

getRawTime = do
    MkCTimespec (CTime s) (CLong ns) <- clockGetTime clock_REALTIME
    return (MkRawTime (fromIntegral s) (fromIntegral ns))
getTime_resolution = case realtimeRes of
    MkCTimespec (CTime s) ns -> (fromIntegral s) + (fromIntegral ns) * 1E-9
getTAIRawTime = Nothing

#else
-- Use gettimeofday
getRawTime = do
    MkCTimeval (CLong s) (CLong us) <- getCTimeval
    return (MkRawTime (fromIntegral s) (fromIntegral us * 1000))
getTime_resolution = 1E-6 -- microsecond
getTAIRawTime = Nothing

#endif
