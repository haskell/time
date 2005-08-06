{-# OPTIONS -ffi -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.Timezone
(
	-- * Time zones
	Timezone(..),timezoneOffsetString,minutesToTimezone,hoursToTimezone,utc,

	-- getting the locale time zone
	getTimezone,getCurrentTimezone
) where

--import System.Time.Calendar.Format
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Foreign
import Foreign.C

-- | A Timezone is a whole number of minutes offset from UTC, together with a name and a "just for summer" flag.
data Timezone = MkTimezone {
	-- | The number of minutes offset from UTC. Positive means local time will be later in the day than UTC.
	timezoneMinutes :: Int,
	-- | Is this time zone just persisting for the summer?
	timezoneDST :: Bool,
	-- | The name of the zone, typically a three- or four-letter acronym.
	timezoneName :: String
} deriving (Eq,Ord)

-- | Create a nameless non-summer timezone for this number of minutes
minutesToTimezone :: Int -> Timezone
minutesToTimezone m = MkTimezone m False ""

-- | Create a nameless non-summer timezone for this number of hours
hoursToTimezone :: Int -> Timezone
hoursToTimezone i = minutesToTimezone (60 * i)

showT :: Int -> String
showT t = (show2 (div t 60)) ++ (show2 (mod t 60))

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like %z in formatTime)
timezoneOffsetString :: Timezone -> String
timezoneOffsetString (MkTimezone t _ _) | t < 0 = '-':(showT (negate t))
timezoneOffsetString (MkTimezone t _ _) = '+':(showT t)

instance Show Timezone where
	show zone@(MkTimezone _ _ "") = timezoneOffsetString zone
	show (MkTimezone _ _ name) = name

-- | The UTC time zone
utc :: Timezone
utc = MkTimezone 0 False "UTC"

foreign import ccall unsafe "timestuff.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> Ptr CInt -> Ptr CString -> IO CLong

posixToCTime :: POSIXTime -> CTime
posixToCTime  = fromInteger . floor

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimezone :: UTCTime -> IO Timezone
getTimezone time = with 0 (\pdst -> with nullPtr (\pcname -> do
	secs <- get_current_timezone_seconds (posixToCTime (utcTimeToPOSIXSeconds time)) pdst pcname
	case secs of
		0x80000000 -> fail "localtime_r failed"
		_ -> do
			dst <- peek pdst
			cname <- peek pcname
			name <- peekCString cname
			return (MkTimezone (div (fromIntegral secs) 60) (dst == 1) name)
	))

-- | Get the current time-zone
getCurrentTimezone :: IO Timezone
getCurrentTimezone = getCurrentTime >>= getTimezone
