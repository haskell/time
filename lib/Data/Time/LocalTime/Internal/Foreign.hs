{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.Foreign (
    getTimeZone,
    getCurrentTimeZone,
) where

import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.LocalTime.Internal.TimeZone
import Foreign
import Foreign.C
#if defined(javascript_HOST_ARCH)
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
#endif

#if defined(javascript_HOST_ARCH)

foreign import javascript "((dy,dm,dd,th,tm,ts) => { return new Date(dy,dm,dd,th,tm,ts).getTimezoneOffset(); })"
  js_get_timezone_minutes :: Int -> Int -> Int -> Int -> Int -> Int -> IO Int

get_timezone_minutes :: UTCTime -> IO Int
get_timezone_minutes ut = let
    lt :: LocalTime
    lt = utcToLocalTime utc ut
    in case lt of
        LocalTime (YearMonthDay dy dm dd) (TimeOfDay th tm ts) ->
            js_get_timezone_minutes (fromInteger dy) (pred dm) dd th tm (floor ts)

getTimeZoneCTime :: CTime -> IO TimeZone
getTimeZoneCTime ct = do
    let
        ut :: UTCTime
        ut = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromIntegral $ fromCTime ct
    mins <- get_timezone_minutes ut
    return $ TimeZone mins False ""

fromCTime :: CTime -> Int64
fromCTime (CTime tt) = fromIntegral tt

#else
{-# CFILES cbits/HsTime.c #-}
foreign import ccall unsafe "HsTime.h get_current_timezone_seconds"
    get_current_timezone_seconds ::
        CTime -> Ptr CInt -> Ptr CString -> IO CLong

getTimeZoneCTime :: CTime -> IO TimeZone
getTimeZoneCTime ctime =
    with 0 $ \pdst ->
        with nullPtr $ \pcname -> do
            secs <- get_current_timezone_seconds ctime pdst pcname
            case secs of
                0x80000000 -> fail "localtime_r failed"
                _ -> do
                    dst <- peek pdst
                    cname <- peek pcname
                    name <- peekCString cname
                    return (TimeZone (div (fromIntegral secs) 60) (dst == 1) name)
#endif

-- there's no instance Bounded CTime, so this is the easiest way to check for overflow
toCTime :: Int64 -> IO CTime
toCTime t =
    let
        tt = fromIntegral t
        t' = fromIntegral tt
    in
        if t' == t
            then return $ CTime tt
            else fail "Data.Time.LocalTime.Internal.TimeZone.toCTime: Overflow"

-- | Get the configured time-zone for a given time (varying as per summertime adjustments).
getTimeZoneSystem :: SystemTime -> IO TimeZone
getTimeZoneSystem t = do
    ctime <- toCTime $ systemSeconds t
    getTimeZoneCTime ctime

-- | Get the configured time-zone for a given time (varying as per summertime adjustments).
--
-- On Unix systems the output of this function depends on:
--
-- 1. The value of @TZ@ environment variable (if set)
--
-- 2. The system time zone (usually configured by @\/etc\/localtime@ symlink)
--
-- For details see tzset(3) and localtime(3).
--
-- Example:
--
-- @
-- > let t = `UTCTime` (`Data.Time.Calendar.fromGregorian` 2021 7 1) 0
-- > `getTimeZone` t
-- CEST
-- > `System.Environment.setEnv` \"TZ\" \"America/New_York\" >> `getTimeZone` t
-- EDT
-- > `System.Environment.setEnv` \"TZ\" \"Europe/Berlin\" >> `getTimeZone` t
-- CEST
-- @
--
-- On Windows systems the output of this function depends on:
--
-- 1. The value of @TZ@ environment variable (if set).
-- See [here](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/tzset) for how Windows interprets this variable.
--
-- 2. The system time zone, configured in Settings
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone t = do
    ctime <- toCTime $ floor $ utcTimeToPOSIXSeconds t
    getTimeZoneCTime ctime

-- | Get the configured time-zone for the current time.
getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = getSystemTime >>= getTimeZoneSystem
