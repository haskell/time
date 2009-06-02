{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Julian
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

checkDay :: (Show t) => (Day -> t) -> (t -> Day) -> (t -> Maybe Day) -> Day -> IO ()
checkDay encodeDay decodeDay decodeDayValid day = do
	let st = encodeDay day
	let day' = decodeDay st
	if day /= day'
	 then putStrLn ((show day) ++ " -> " ++ (show st) ++ " -> " ++ (show day') ++ " (diff " ++ (show (diffDays day' day)) ++ ")")
	 else return ()
	let mday' = decodeDayValid st
	if Just day /= mday'
	 then putStrLn ((show day) ++ " -> " ++ (show st) ++ " -> " ++ (show mday'))
	 else return ()
		

checkers :: [Day -> IO ()]
checkers = [
	checkDay toOrdinalDate (\(y,d) -> fromOrdinalDate y d) (\(y,d) -> fromOrdinalDateValid y d),
	checkDay toWeekDate (\(y,w,d) -> fromWeekDate y w d) (\(y,w,d) -> fromWeekDateValid y w d),
	checkDay toGregorian (\(y,m,d) -> fromGregorian y m d) (\(y,m,d) -> fromGregorianValid y m d),
	checkDay toJulian (\(y,m,d) -> fromJulian y m d) (\(y,m,d) -> fromJulianValid y m d)
	]

days :: [Day]
days = [ModifiedJulianDay 50000 .. ModifiedJulianDay 50200] ++
	(fmap (\year -> (fromGregorian year 1 4)) [1980..2000])

main :: IO ()
main = mapM_ (\ch -> mapM_ ch days) checkers
