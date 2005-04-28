module Main where

import System.Time.Clock
import System.Time.TAI
import System.Time.Calendar

showCal :: ModJulianDay -> IO ()
showCal d = do
	let cal = dayToCalendar d
	let d' = calendarToDay cal
	putStr ((show d) ++ "=" ++ show (dayToCalendar d))
	putStrLn (if d == d' then "" else "=" ++ (show d') ++ "!")


testCal :: IO ()
testCal = do
	showCal 0	
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
	for showCal [51540..52280]	

showUTCTime :: UTCTime -> String
showUTCTime (UTCTime d t) =  show d ++ "," ++ show t

for :: (Monad m) => (a -> m ()) -> [a] -> m ()
for _ [] = return ()
for f (x:xs) = f x >> for f xs

myzone :: Timezone
myzone = hoursToTimezone (- 8)

leapSec1998Cal :: CalendarTime
leapSec1998Cal = CalendarTime (CalendarDay 1998 12 31) (TimeOfDay 23 59 60.5)

leapSec1998 :: UTCTime
leapSec1998 = calendarToUTC utc leapSec1998Cal

testUTC :: IO ()
testUTC = do
	putStrLn ""
	showCal 51178
	putStrLn (show leapSec1998Cal)
	putStrLn (showUTCTime leapSec1998)
	let lsMineCal = utcToCalendar myzone leapSec1998
	putStrLn (show lsMineCal)
	let lsMine = calendarToUTC myzone lsMineCal
	putStrLn (showUTCTime lsMine)

neglong :: Rational
neglong = -120

poslong :: Rational
poslong = 120

testUT1 :: IO ()
testUT1 = do
	putStrLn ""
	putStrLn (show (ut1ToCalendar 0 51604.0))
	putStrLn (show (ut1ToCalendar 0 51604.5))
	putStrLn (show (ut1ToCalendar neglong 51604.0))
	putStrLn (show (ut1ToCalendar neglong 51604.5))
	putStrLn (show (ut1ToCalendar poslong 51604.0))
	putStrLn (show (ut1ToCalendar poslong 51604.5))

main :: IO ()
main = do
	testCal
	testUTC
	testUT1
