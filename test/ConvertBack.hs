{-# OPTIONS -Wall -Werror #-}

module Main where

import System.Time.Calendar
import System.Time.Clock

checkDay :: ModJulianDay -> IO ()
checkDay day = do
	let st = encodeDay day :: YearDay
	let day' = decodeDay st
	if day /= day'
	 then putStrLn ((show day) ++ " -> " ++ (show st) ++ " -> " ++ (show day'))
	 else return ()


main :: IO ()
main = do
	mapM_ checkDay [50000..50200]
	mapM_ (\year -> checkDay (decodeDay (GregorianDay year 1 4))) [1980..2000]
