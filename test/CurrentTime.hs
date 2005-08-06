{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time

main :: IO ()
main = do
	now <- getCurrentTime
	putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
	putStrLn (show (zonedTimeFromUTC utc now :: ZonedTime))
	myzone <- getCurrentTimeZone
	putStrLn (show (zonedTimeFromUTC myzone now :: ZonedTime))
