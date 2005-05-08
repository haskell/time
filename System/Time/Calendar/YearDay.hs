{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.YearDay where

import System.Time.Calendar.Calendar
import System.Time.Calendar.Private

-- | ISO 8601 Ordinal Date
data YearDay = YearDay {
	ydYear  :: Integer,
	ydDay   :: Int
} deriving (Eq,Ord)

instance Show YearDay where
	show (YearDay y d) = (show y) ++ "-" ++ (show3 d)

instance DayEncoding YearDay where
	encodeDay mjd = YearDay year yd where
		a = mjd + 678575
		quadcent = div a 146097
		b = mod a 146097
		cent = min (div b 36524) 3
		c = b - (cent * 36524)
		quad = div c 1461
		d = mod c 1461
		y = min (div d 365) 3
		yd = fromInteger (d - (y * 365) + 1)
		year = quadcent * 400 + cent * 100 + quad * 4 + y + 1
	decodeDay (YearDay year day) =
		(fromIntegral day) + (div (1532) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		y = year - 1
	maybeDecodeDay t@(YearDay year day) | (day >= 1) && (day <= if isLeapYear year then 366 else 365) = Just (decodeDay t)
	maybeDecodeDay _ = Nothing

isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))
