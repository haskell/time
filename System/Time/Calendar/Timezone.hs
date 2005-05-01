{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Calendar.Timezone
(
	-- time zones
	Timezone(..),minutesToTimezone,hoursToTimezone,utc,

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
	timezoneDST :: Bool,
	timezoneMinutes :: Int
} deriving (Eq,Ord)

minutesToTimezone :: Int -> Timezone
minutesToTimezone = MkTimezone False

hoursToTimezone :: Int -> Timezone
hoursToTimezone i = minutesToTimezone (60 * i)

showT :: Int -> String
showT t = (show2 (div t 60)) ++ (show2 (mod t 60))

instance Show Timezone where
	show (MkTimezone _ t) | t < 0 = '-':(showT (negate t))
	show (MkTimezone _ t) = '+':(showT t)

instance FormatTime Timezone where
	formatCharacter _ 'z' zone = Just (show zone)
	formatCharacter _ _ _ = Nothing

-- | The UTC time zone
utc :: Timezone
utc = minutesToTimezone 0

foreign import ccall unsafe "timestuff.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> Ptr CInt -> IO CLong

posixToCTime :: POSIXTime -> CTime
posixToCTime  = fromInteger . floor

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimezone :: UTCTime -> IO Timezone
getTimezone time = with 0 (\pdst -> do
	secs <- get_current_timezone_seconds (posixToCTime (utcTimeToPOSIXSeconds time)) pdst
	case secs of
		0x80000000 -> fail "localtime_r failed"
		_ -> do
			dst <- peek pdst
			return (MkTimezone (dst == 1) (div (fromIntegral secs) 60))
	)

-- | Get the current time-zone
getCurrentTimezone :: IO Timezone
getCurrentTimezone = getCurrentTime >>= getTimezone
