{-# OPTIONS -ffi -Wall -Werror #-}

module Main where

import System.Time.Calendar
import System.Time.Clock

import System.Locale
import Foreign
import Foreign.C

{-
	size_t format_time (
	char *s, size_t maxsize,
	const char *format,
	int isdst,int gmtoff,time_t t);
-}

foreign import ccall unsafe "TestFormatStuff.h format_time" format_time :: CString -> CSize -> CString -> CInt -> CInt -> CString -> CTime -> IO CSize

withBuffer :: Int -> (CString -> IO CSize) -> IO String
withBuffer n f = withArray (replicate n 0) (\buffer -> do
			len <- f buffer
			peekCStringLen (buffer,fromIntegral len)
		)

unixFormatTime :: String -> Timezone -> UTCTime -> IO String
unixFormatTime fmt zone time = withCString fmt (\pfmt -> withCString (timezoneName zone) (\pzonename ->
		withBuffer 100 (\buffer -> format_time buffer 100 pfmt
				(if timezoneDST zone then 1 else 0)
				(fromIntegral (timezoneMinutes zone * 60))
				pzonename
				(fromInteger (truncate (utcTimeToPOSIXSeconds time)))
			)
		))

locale :: TimeLocale
locale = defaultTimeLocale {dateTimeFmt = "%a %b %e %H:%M:%S %Y"}

zones :: [Timezone]
zones = [utc,hoursToTimezone (- 7)]

posixDay :: UTCDiffTime
posixDay = 86400

baseTime0 :: UTCTime
baseTime0 = decodeLocalUTC utc (CalendarTime (GregorianDay 1970 01 01) midnight)

baseTime1 :: UTCTime
baseTime1 = decodeLocalUTC utc (CalendarTime (GregorianDay 2000 01 01) midnight)

getDay :: Integer -> UTCTime
getDay day = addUTCTime ((fromInteger day) * posixDay) baseTime1

getYearP1 :: Integer -> UTCTime
getYearP1 year = decodeLocalUTC utc (CalendarTime (GregorianDay year 01 01) midnight)

getYearP2 :: Integer -> UTCTime
getYearP2 year = decodeLocalUTC utc (CalendarTime (GregorianDay year 02 04) midnight)

getYearP3 :: Integer -> UTCTime
getYearP3 year = decodeLocalUTC utc (CalendarTime (GregorianDay year 03 04) midnight)

getYearP4 :: Integer -> UTCTime
getYearP4 year = decodeLocalUTC utc (CalendarTime (GregorianDay year 12 31) midnight)

times :: [UTCTime]
times = [baseTime0] ++ (fmap getDay [0..23])

compareFormat :: String -> Timezone -> UTCTime -> IO ()
compareFormat fmt zone time = let
		ctime = encodeUTC zone time :: ZonedGregorianTime
		haskellText = formatTime locale fmt ctime
	in do
		unixText <- unixFormatTime fmt zone time
		if haskellText == unixText then return () else
			putStrLn ("Mismatch with " ++ fmt ++ " for " ++ (show ctime) ++ ": UNIX=\"" ++ unixText ++ "\", TimeLib=\"" ++ haskellText ++ "\".")

-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

main :: IO ()
main = do
	mapM_ (\day -> compareFormat "%G-W%V-%u" utc (getDay day)) [0..100]
	mapM_ (\year -> compareFormat "%G-W%V-%u" utc (getYearP1 year)) [1980..2000]
	mapM_ (\year -> compareFormat "%G-W%V-%u" utc (getYearP2 year)) [1980..2000]
	mapM_ (\year -> compareFormat "%G-W%V-%u" utc (getYearP3 year)) [1980..2000]
	mapM_ (\year -> compareFormat "%G-W%V-%u" utc (getYearP4 year)) [1980..2000]
	mapM_ (\char -> let fmt = '%':char:[] in mapM_ (\time -> mapM_ (\zone -> compareFormat fmt zone time) zones) times) chars
