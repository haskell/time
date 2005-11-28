{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time

showCal :: Integer -> IO ()
showCal mjd = do
	let date = ModifiedJulianDay mjd
	let (y,m,d) = toGregorian date
	let date' = fromGregorian y m d
	putStr ((show mjd) ++ "=" ++ (showGregorian date) ++ "=" ++ (showOrdinalDate date) ++ "=" ++ (showWeekDate date))
	putStrLn (if date == date' then "" else "=" ++ (show (toModifiedJulianDay date')) ++ "!")

testCal :: IO ()
testCal = do
	-- days around 1 BCE/1 CE
	mapM_ showCal [-678950 .. -678930]	
	-- days around 1000 CE
	mapM_ showCal [-313710 .. -313690]	
	-- days around MJD zero
	mapM_ showCal [-30..30]	
	showCal 40000
	showCal 50000
	-- 1900 not a leap year
	showCal 15078
	showCal 15079
	-- 1980 is a leap year
	showCal 44297
	showCal 44298
	showCal 44299
	-- 1990 not a leap year
	showCal 47950
	showCal 47951
	-- 2000 is a leap year
	showCal 51602
	showCal 51603
	showCal 51604
	-- years 2000 and 2001, plus some slop
	mapM_ showCal [51540..52280]	

showUTCTime :: UTCTime -> String
showUTCTime (UTCTime d t) =  show (toModifiedJulianDay d) ++ "," ++ show t

myzone :: TimeZone
myzone = hoursToTimeZone (- 8)

leapSec1998Cal :: LocalTime
leapSec1998Cal = LocalTime (fromGregorian 1998 12 31) (TimeOfDay 23 59 60.5)

leapSec1998 :: UTCTime
leapSec1998 = localTimeToUTC utc leapSec1998Cal

testUTC :: IO ()
testUTC = do
	putStrLn ""
	showCal 51178
	putStrLn (show leapSec1998Cal)
	putStrLn (showUTCTime leapSec1998)
	let lsMineCal = utcToLocalTime myzone leapSec1998
	putStrLn (show lsMineCal)
	let lsMine = localTimeToUTC myzone lsMineCal
	putStrLn (showUTCTime lsMine)

neglong :: Rational
neglong = -120

poslong :: Rational
poslong = 120

testUT1 :: IO ()
testUT1 = do
	putStrLn ""
	putStrLn (show (ut1ToLocalTime 0 (ModJulianDate 51604.0)))
	putStrLn (show (ut1ToLocalTime 0 (ModJulianDate 51604.5)))
	putStrLn (show (ut1ToLocalTime neglong (ModJulianDate 51604.0)))
	putStrLn (show (ut1ToLocalTime neglong (ModJulianDate 51604.5)))
	putStrLn (show (ut1ToLocalTime poslong (ModJulianDate 51604.0)))
	putStrLn (show (ut1ToLocalTime poslong (ModJulianDate 51604.5)))

main :: IO ()
main = do
	testCal
	testUTC
	testUT1
