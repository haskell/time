{-# LANGUAGE CPP #-}

#if !defined(__MHS__)
#include "HsTimeConfig.h"
#endif

#if defined(mingw32_HOST_OS) || !defined(HAVE_CLOCK_GETTIME)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Time.Clock.Internal.SystemTime (
    SystemTime (..),
    getSystemTime,
    getTime_resolution,
    getTAISystemTime,
) where

import Control.DeepSeq
import Data.Data
import Data.Int (Int64)
import Data.Time.Clock.Internal.DiffTime
import Data.Word
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH

#ifdef mingw32_HOST_OS
import qualified System.Win32.Time as Win32
#elif defined(HAVE_CLOCK_GETTIME) && !defined(__MHS__)
import Data.Time.Clock.Internal.CTimespec
import Foreign.C.Types (CLong(..), CTime(..))
#else
import Data.Time.Clock.Internal.CTimeval
import Foreign.C.Types (CLong(..))
#endif
--------------------------------------------------------------------------------

-- | 'SystemTime' is time returned by system clock functions.
-- Its semantics depends on the clock function, but the epoch is typically the beginning of 1970.
-- Note that 'systemNanoseconds' of 1E9 to 2E9-1 can be used to represent leap seconds.
data SystemTime = MkSystemTime
    { systemSeconds :: {-# UNPACK #-} !Int64
    , systemNanoseconds :: {-# UNPACK #-} !Word32
    }
    deriving (Eq, Ord, Show, Typeable, Data, Generic, TH.Lift)

instance NFData SystemTime where
    rnf a = a `seq` ()

-- | Get the system time, epoch start of 1970 UTC, leap-seconds ignored.
-- 'getSystemTime' is typically much faster than 'getCurrentTime'.
getSystemTime :: IO SystemTime

-- | The resolution of 'getSystemTime', 'getCurrentTime', 'getPOSIXTime'.
-- On UNIX systems this uses @clock_getres@, which may be <https://github.com/microsoft/WSL/issues/6029 wrong on WSL2>.
getTime_resolution :: DiffTime

-- | If supported, get TAI time, epoch start of 1970 TAI, with resolution.
-- This is supported only on UNIX systems, and only those with CLOCK_TAI available at run-time.
getTAISystemTime :: Maybe (DiffTime, IO SystemTime)

#ifdef mingw32_HOST_OS
-- On Windows, the equivalent of POSIX time is "file time", defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.
getSystemTime = do
    Win32.FILETIME ft <- Win32.getSystemTimeAsFileTime
    let (s, us) = (ft - win32_epoch_adjust) `divMod` 10000000
    return (MkSystemTime (fromIntegral s) (fromIntegral us * 100))
  where
    win32_epoch_adjust :: Word64
    win32_epoch_adjust = 116444736000000000

getTime_resolution = 100E-9 -- 100ns

getTAISystemTime = Nothing
#elif defined(HAVE_CLOCK_GETTIME) && !defined(__MHS__)
-- Use hi-res clock_gettime
timespecToSystemTime :: CTimespec -> SystemTime
timespecToSystemTime (MkCTimespec (CTime s) (CLong ns)) = (MkSystemTime (fromIntegral s) (fromIntegral ns))

timespecToDiffTime :: CTimespec -> DiffTime
timespecToDiffTime (MkCTimespec (CTime s) ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9

clockGetSystemTime :: ClockID -> IO SystemTime
clockGetSystemTime clock = fmap timespecToSystemTime $ clockGetTime clock

getSystemTime = clockGetSystemTime clock_REALTIME

getTime_resolution = timespecToDiffTime realtimeRes

getTAISystemTime = do
    clockID <- clock_TAI
    resolution <- clockResolution clockID
    return $ (timespecToDiffTime resolution, clockGetSystemTime clockID)
#else
-- Use gettimeofday
getSystemTime = do
    MkCTimeval (CLong s) (CLong us) <- getCTimeval
    return (MkSystemTime (fromIntegral s) (fromIntegral us * 1000))

getTime_resolution = 1E-6 -- microsecond

getTAISystemTime = Nothing
#endif
