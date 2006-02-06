{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.Julian
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

showers :: [(String,Day -> String)]
showers = [
	("MJD",show . toModifiedJulianDay),
	("Gregorian",showGregorian),
	("Julian",showJulian),
	("ISO 8601",showWeekDate)
	]

days :: [Day]
days = [
	fromGregorian 0 12 31,
	fromJulian 1752 9 2,
	fromGregorian 1752 9 14,
	fromGregorian 2005 1 23
	]

main :: IO ()
main = mapM_ (\day -> do
	mapM_ (\(name,shower) -> putStr  (" == " ++ name ++ " " ++ (shower day))) showers
	putStrLn ""
	) days
