module Main where

import System.Time.Clock
import System.Time.TAI
import System.Time.Calendar

main :: IO ()
main = do
	now <- getCurrentTime
	putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
	putStrLn (show (encodeUTC utc now :: ZonedTime))
	myzone <- getCurrentTimezone
	putStrLn (show (encodeUTC myzone now :: ZonedTime))
