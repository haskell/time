{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Calendar.Timezone
(
	-- time zones
	Timezone(..),timezoneOffsetString,minutesToTimezone,hoursToTimezone,utc,

	-- getting the locale time zone
	getTimezone,getCurrentTimezone
) where

import System.Time.Calendar.Format
import System.Time.Calendar.Private
import System.Time.Clock

import Foreign
import Foreign.C

-- | count of minutes
data Timezone = MkTimezone {
	timezoneMinutes :: Int,
	timezoneDST :: Bool,
	timezoneName :: String
} deriving (Eq,Ord)

minutesToTimezone :: Int -> Timezone
minutesToTimezone m = MkTimezone m False ""

hoursToTimezone :: Int -> Timezone
hoursToTimezone i = minutesToTimezone (60 * i)

showT :: Int -> String
showT t = (show2 (div t 60)) ++ (show2 (mod t 60))

timezoneOffsetString :: Timezone -> String
timezoneOffsetString (MkTimezone t _ _) | t < 0 = '-':(showT (negate t))
timezoneOffsetString (MkTimezone t _ _) = '+':(showT t)

instance Show Timezone where
	show zone@(MkTimezone _ _ "") = timezoneOffsetString zone
	show (MkTimezone _ _ name) = name

instance FormatTime Timezone where
	formatCharacter 'z' = Just (\_ -> timezoneOffsetString)
	formatCharacter 'Z' = Just (\_ -> timezoneName)
	formatCharacter _ = Nothing

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
