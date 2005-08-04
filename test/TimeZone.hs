module Main where

import Data.Time.Clock
import Data.Time.Calendar

main :: IO ()
main = do
	zone <- getCurrentTimezone
	putStrLn (timezoneOffsetString zone)
