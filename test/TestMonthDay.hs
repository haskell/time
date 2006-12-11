{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.MonthDay

showCompare :: (Eq a,Show a) => a -> String -> a -> String
showCompare a1 b a2 | a1 == a2 = (show a1) ++ " == " ++ b
showCompare a1 b a2 = "DIFF: " ++ (show a1) ++ " -> " ++ b ++ " -> " ++ (show a2)

main :: IO ()
main = mapM_ (\isLeap -> do
	putStrLn (if isLeap then "Leap:" else "Regular:")
	mapM_ (\yd -> do
		let (m,d) = dayOfYearToMonthAndDay isLeap yd
		let yd' = monthAndDayToDayOfYear isLeap m d
		let mdtext = (show m) ++ "-" ++ (show d)
		putStrLn (showCompare yd mdtext yd')
		) [-2..369]
	) [False,True]
