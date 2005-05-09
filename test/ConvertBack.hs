{-# OPTIONS -Wall -Werror #-}

module Main where

import System.Time.Calendar
import System.Time.Clock

checkDay :: (DayEncoding t,Show t) => t -> ModJulianDay -> IO ()
checkDay t day = do
	let st = encodeDay' t day
	let day' = decodeDay st
	if day /= day'
	 then putStrLn ((show day) ++ " -> " ++ (show st) ++ " -> " ++ (show day') ++ " (diff " ++ (show (day' - day)) ++ ")")
	 else return ()
	where
		encodeDay' :: (DayEncoding t,Show t) => t -> ModJulianDay -> t
		encodeDay' _ = encodeDay

checkers :: [ModJulianDay -> IO ()]
checkers = [
	checkDay (undefined :: YearDay),
	checkDay (undefined :: ISOWeek),
	checkDay (undefined :: GregorianDay)
	]

days :: [ModJulianDay]
days = [50000..50200] ++
	(fmap (\year -> (decodeDay (GregorianDay year 1 4))) [1980..2000])

main :: IO ()
main = mapM_ (\ch -> mapM_ ch days) checkers
