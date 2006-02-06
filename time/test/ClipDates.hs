{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar
import Control.Monad

checkYearAndDay :: (Integer,Int) -> IO ()
checkYearAndDay (y,d) = putStrLn ((show y) ++ "-" ++ (show d) ++ " = " ++ (showOrdinalDate (fromOrdinalDate y d)))

checkGregorian :: (Integer,Int,Int) -> IO ()
checkGregorian (y,m,d) = putStrLn ((show y) ++ "-" ++ (show m) ++ "-" ++ (show d) ++ " = " ++ (showGregorian (fromGregorian y m d)))

checkISOWeekDay :: (Integer,Int,Int) -> IO ()
checkISOWeekDay (y,w,d) = putStrLn ((show y) ++ "-W" ++ (show w) ++ "-" ++ (show d) ++ " = " ++ (showWeekDate (fromWeekDate y w d)))

main :: IO ()
main = do
	putStrLn "YearAndDay"
	mapM_ checkYearAndDay (liftM2 (,)  [1968,1969,1971] [-4,0,1,200,364,365,366,367,700])
	putStrLn "Gregorian"
	mapM_ checkGregorian  (liftM3 (,,) [1968,1969,1971] [-20,-1,0,1,2,12,13,17] [-7,-1,0,1,2,27,28,29,30,31,32,40])
	putStrLn "ISOWeekDay"
	mapM_ checkISOWeekDay (liftM3 (,,) [1968,1969,2004] [-20,-1,0,1,20,51,52,53,54] [-2,-1,0,1,4,6,7,8,9])
