{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.ISOWeekDay
	(
	-- * ISO Week calendar
	module Data.Time.Calendar.ISOWeekDay
	) where

import Data.Time.Calendar.YearDay
import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

showISOWeekDay :: Date -> String
showISOWeekDay date = (show4 y) ++ "-W" ++ (show2 w) ++ "-" ++ (show d) where
	(y,w,d) = isoWeekDay date

isoWeekDay :: Date -> (Integer,Int,Int)
isoWeekDay date@(ModJulianDay mjd) = (y1,fromInteger (w1 + 1),fromInteger (mod d 7) + 1) where
	(y0,yd) = yearAndDay date
	d = mjd + 2
	foo :: Integer -> Integer
	foo y = bar (getModJulianDay (fromYearAndDay y 6))
	bar k = (div d 7) - (div k 7)
	w0 = bar (d - (toInteger yd) + 4)
	(y1,w1) = if w0 == -1
		then (y0 - 1,foo (y0 - 1))
		else if w0 == 52
		then if (foo (y0 + 1)) == 0
			then (y0 + 1,0)
			else (y0,w0)
		else (y0,w0)

fromISOWeekDay :: Integer -> Int -> Int -> Date
fromISOWeekDay y w d = ModJulianDay (k - (mod k 7) + (toInteger ((w * 7) + d)) - 10) where
		k = getModJulianDay (fromYearAndDay y 6)
