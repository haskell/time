{-# OPTIONS -ffi -Wall -Werror #-}

module Main where

import Data.Time.Calendar.Easter
import Data.Time.Calendar
import Data.Time.LocalTime

import System.Locale

days :: [Day]
days = [ModifiedJulianDay 53000 .. ModifiedJulianDay 53014]

showWithWDay :: Day -> String
showWithWDay = formatTime defaultTimeLocale "%F %A"

main :: IO ()
main = do
	mapM_ (\day -> putStrLn ((showWithWDay day) ++ " -> " ++ (showWithWDay (sundayAfter day)))) days
	mapM_ (\year -> do
		putStrLn ((show year) ++ ", Gregorian: moon, " ++ (show (gregorianPaschalMoon year)) ++ ": Easter, " ++ (showWithWDay (gregorianEaster year)))
		putStrLn ((show year) ++ ", Orthodox : moon, " ++ (show (orthodoxPaschalMoon year)) ++ ": Easter, " ++ (showWithWDay (orthodoxEaster year)))
		) [2000..2020]
