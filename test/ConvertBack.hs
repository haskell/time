{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar

checkDay :: (Show t) => (Day -> t) -> (t -> Day) -> Day -> IO ()
checkDay encodeDay decodeDay day = do
	let st = encodeDay day
	let day' = decodeDay st
	if day /= day'
	 then putStrLn ((show day) ++ " -> " ++ (show st) ++ " -> " ++ (show day') ++ " (diff " ++ (show (diffDays day' day)) ++ ")")
	 else return ()

checkers :: [Day -> IO ()]
checkers = [
	checkDay toYearAndDay (\(y,d) -> fromYearAndDay y d),
	checkDay toISOWeekDay (\(y,w,d) -> fromISOWeekDay y w d),
	checkDay toGregorian (\(y,m,d) -> fromGregorian y m d)
	]

days :: [Day]
days = [ModifiedJulianDay 50000 .. ModifiedJulianDay 50200] ++
	(fmap (\year -> (fromGregorian year 1 4)) [1980..2000])

main :: IO ()
main = mapM_ (\ch -> mapM_ ch days) checkers
