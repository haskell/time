module Main where

import System.Time.Clock
import System.Time.TAI
import System.Time.Calendar

myzone :: TimeZone
myzone = hoursToTimezone (- 8)

main :: IO ()
main = do
	now <- getCurrentTime
	putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
	putStrLn (show (utcToCalendar myzone now))
