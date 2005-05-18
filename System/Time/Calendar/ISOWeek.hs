{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.ISOWeek
	(
	-- * ISO Week calendar
	module System.Time.Calendar.ISOWeek
	) where

import System.Time.Calendar.YearDay
import System.Time.Calendar.Calendar
import System.Time.Calendar.Private

-- | Year, week and day aggregate for ISO week count
data ISOWeek = ISOWeek {
	isowYear  :: Integer,
	isowWeek  :: Int,
	isowDay   :: Int
} deriving (Eq,Ord)

instance Show ISOWeek where
	show (ISOWeek y w d) = (show y) ++ "-W" ++ (show2 w) ++ "-" ++ (show d)

instance DayEncoding ISOWeek where
	encodeDay mjd = ISOWeek y1 (fromInteger (w1 + 1)) (fromInteger (mod d 7) + 1) where
		(YearDay y0 yd) = encodeDay mjd
		d = mjd + 2
		foo :: Integer -> Integer
		foo y = bar (decodeDay (YearDay y 6))
		bar k = (div d 7) - (div k 7)
		w0 = bar (d - (toInteger yd) + 4)
		(y1,w1) = if w0 == -1
			then (y0 - 1,foo (y0 - 1))
			else if w0 == 52
			then if (foo (y0 + 1)) == 0
				then (y0 + 1,0)
				else (y0,w0)
			else (y0,w0)

	decodeDay (ISOWeek y w d) = k - (mod k 7) + (toInteger ((w * 7) + d)) - 10 where
		k = decodeDay (YearDay y 6)
