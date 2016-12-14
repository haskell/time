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
    fromRational r = let
        (s,ns) = divMod (floor $ r * 1000000000)  1000000000
        in POSIXTime (fromInteger s) (fromInteger ns)
    recip = error "undefined POSIXTime function"
    (/) = error "undefined POSIXTime function"
#endif

instance NFData POSIXTime where
    rnf a = a `seq` ()


getPOSIXTime :: IO POSIXTime
getTime_resolution :: DiffTime
getTAIRawTime :: Maybe (DiffTime,IO POSIXTime)

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
getTime_resolution = 1E-6 -- microsecond
getTAIRawTime = Nothing

#elif HAVE_CLOCK_GETTIME
-- Use hi-res clock_gettime

getPOSIXTime = do
    MkCTimespec (CTime s) (CLong ns) <- clockGetTime clock_REALTIME
    return (POSIXTime (fromIntegral s) (fromIntegral ns))
getTime_resolution = case realtimeRes of
    MkCTimespec (CTime s) ns -> (fromIntegral s) + (fromIntegral ns) * 1E-9
getTAIRawTime = Nothing

#else
-- Use gettimeofday
getPOSIXTime = do
    MkCTimeval (CLong s) (CLong us) <- getCTimeval
    return (POSIXTime (fromIntegral s) (fromIntegral us * 1000))
getTime_resolution = 1E-6 -- microsecond
getTAIRawTime = Nothing

#endif
