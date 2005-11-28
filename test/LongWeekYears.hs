{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

longYear :: Integer -> Bool
longYear year = case toWeekDate (fromGregorian year 12 31) of
	(_,53,_) -> True
	_ -> False

showLongYear :: Integer -> IO ()
showLongYear year = putStrLn ((show year) ++ ": " ++ (if isLeapYear year then "L" else " ") ++ (if longYear year then "*" else " "))

main :: IO ()
main = do
	mapM_ showLongYear [1901 .. 2050]
