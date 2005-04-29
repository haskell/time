{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Calendar.Timezone
(
	-- time zones
	Timezone,timezoneToMinutes,minutesToTimezone,hoursToTimezone,utc,

	-- getting the locale time zone
	getTimezone,getCurrentTimezone,
	
	show2
) where

import System.Time.Calendar.Private
import System.Time.Clock

import Foreign
import Foreign.C

-- | count of minutes
newtype Timezone = MkTimezone {
	timezoneToMinutes :: Int
} deriving (Eq,Ord)

minutesToTimezone :: Int -> Timezone
minutesToTimezone = MkTimezone

hoursToTimezone :: Int -> Timezone
hoursToTimezone i = minutesToTimezone (60 * i)

instance Show Timezone where
	show (MkTimezone t) | t < 0 = '-':(show (MkTimezone (negate t)))
	show (MkTimezone t) = (show2 (div t 60)) ++ (show2 (mod t 60))

-- | The UTC time zone
utc :: Timezone
utc = minutesToTimezone 0

foreign import ccall unsafe "timestuff.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> IO CLong

posixToCTime :: POSIXTime -> CTime
posixToCTime  = fromInteger . floor

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimezone :: UTCTime -> IO Timezone
getTimezone time = do
	secs <- get_current_timezone_seconds (posixToCTime (utcTimeToPOSIXSeconds time))
	case secs of
		0x80000000 -> fail "localtime_r failed"
		_ -> return (minutesToTimezone (div (fromIntegral secs) 60))

-- | Get the current time-zone
getCurrentTimezone :: IO Timezone
getCurrentTimezone = getCurrentTime >>= getTimezone
