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

for :: (Monad m) => (a -> m ()) -> [a] -> m ()
for _ [] = return ()
for f (x:xs) = f x >> for f xs

main :: IO ()
main = do
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
