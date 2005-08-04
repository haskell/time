module Main where

import Data.Time.Clock
import Data.Time.TAI
import Data.Time.Calendar

main :: IO ()
main = do
	now <- getCurrentTime
	putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
	putStrLn (show (encodeUTC utc now :: ZonedTime))
	myzone <- getCurrentTimezone
	putStrLn (show (encodeUTC myzone now :: ZonedTime))
