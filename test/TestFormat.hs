{-# OPTIONS -ffi -Wall -Werror #-}

module Main where

import Data.Time
import Data.Time.Clock.POSIX

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

unixFormatTime :: String -> TimeZone -> UTCTime -> IO String
unixFormatTime fmt zone time = withCString fmt (\pfmt -> withCString (timeZoneName zone) (\pzonename ->
		withBuffer 100 (\buffer -> format_time buffer 100 pfmt
				(if timeZoneSummerOnly zone then 1 else 0)
				(fromIntegral (timeZoneMinutes zone * 60))
				pzonename
				(fromInteger (truncate (utcTimeToPOSIXSeconds time)))
			)
		))

locale :: TimeLocale
locale = defaultTimeLocale {dateTimeFmt = "%a %b %e %H:%M:%S %Y"}

zones :: [TimeZone]
zones = [utc,TimeZone 87 True "Fenwickian Daylight Time"]

baseTime0 :: UTCTime
baseTime0 = localTimeToUTC utc (LocalTime (fromGregorian 1970 01 01) midnight)

baseTime1 :: UTCTime
baseTime1 = localTimeToUTC utc (LocalTime (fromGregorian 2000 01 01) midnight)

getDay :: Integer -> UTCTime
getDay day = addUTCTime ((fromInteger day) * posixDayLength) baseTime1

getYearP1 :: Integer -> UTCTime
getYearP1 year = localTimeToUTC utc (LocalTime (fromGregorian year 01 01) midnight)

getYearP2 :: Integer -> UTCTime
getYearP2 year = localTimeToUTC utc (LocalTime (fromGregorian year 02 04) midnight)

getYearP3 :: Integer -> UTCTime
getYearP3 year = localTimeToUTC utc (LocalTime (fromGregorian year 03 04) midnight)

getYearP4 :: Integer -> UTCTime
getYearP4 year = localTimeToUTC utc (LocalTime (fromGregorian year 12 31) midnight)

times :: [UTCTime]
times = [baseTime0] ++ (fmap getDay [0..23]) ++ (fmap getDay [0..100]) ++
	(fmap getYearP1 [1980..2000]) ++ (fmap getYearP2 [1980..2000]) ++ (fmap getYearP3 [1980..2000]) ++ (fmap getYearP4 [1980..2000])

compareFormat :: String -> TimeZone -> UTCTime -> IO ()
compareFormat fmt zone time = let
		ctime = utcToZonedTime zone time
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

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"] ++ (fmap (\char -> '%':char:[]) chars)

main :: IO ()
main = mapM_ (\fmt -> mapM_ (\time -> mapM_ (\zone -> compareFormat fmt zone time) zones) times) formats
