module Main where

import System.Time.Clock
import System.Time.Calendar

monthBeginning :: Timezone -> Integer -> Int -> UTCTime
monthBeginning zone year month = calendarToUTC zone
	(CalendarTime (CalendarDay year month 1) midnight)

findTransition :: UTCTime -> UTCTime -> IO [(UTCTime,Timezone,Timezone)]
findTransition a b = do
	za <- getTimezone a
	zb <- getTimezone b
	if za == zb then return [] else do
		let c = addUTCTime ((diffUTCTime b a) / 2) a
		if a == c then return [(b,za,zb)] else do
			tp <- findTransition a c
			tq <- findTransition c b
			return (tp ++ tq)

showZoneTime :: Timezone -> UTCTime -> String
showZoneTime zone time = (show (utcToCalendar zone time)) ++ " " ++ (show zone)

showTransition :: (UTCTime,Timezone,Timezone) -> String
showTransition (time,zone1,zone2) = (showZoneTime zone1 time) ++ " => " ++ (showZoneTime zone2 time)

main :: IO ()
main = do
	now <- getCurrentTime
	zone <- getTimezone now
	let year = cdYear (ctDay (utcToCalendar zone now))
	putStrLn ("DST adjustments for " ++ show year ++ ":")
	let t0 = monthBeginning zone year 1
	let t1 = monthBeginning zone year 4
	let t2 = monthBeginning zone year 7
	let t3 = monthBeginning zone year 10
	let t4 = monthBeginning zone (year + 1) 1
	tr1 <- findTransition t0 t1
	tr2 <- findTransition t1 t2
	tr3 <- findTransition t2 t3
	tr4 <- findTransition t3 t4
	mapM_ (putStrLn . showTransition) (tr1 ++ tr2 ++ tr3 ++ tr4)
