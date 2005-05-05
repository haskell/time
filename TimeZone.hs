module Main where

import System.Time.Clock
import System.Time.Calendar

main :: IO ()
main = do
	zone <- getCurrentTimezone
	putStrLn (timezoneOffsetString zone)
