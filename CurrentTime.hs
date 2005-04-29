module Main where

import System.Time.Clock
import System.Time.TAI
import System.Time.Calendar

main :: IO ()
main = do
	now <- getCurrentTime
	putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
	putStrLn (show (utcToCalendar utc now :: GregorianTime))
	myzone <- getCurrentTimezone
	putStrLn ("timezone: " ++ show myzone)
	putStrLn (show (utcToCalendar myzone now :: GregorianTime))
