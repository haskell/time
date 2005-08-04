{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.Days
(
	-- * Days
	Date(..),addDate,diffDate
) where

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
newtype Date = ModJulianDay {getModJulianDay :: Integer} deriving (Eq,Ord)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Enum Date where
	succ (ModJulianDay a) = ModJulianDay (succ a)
	pred (ModJulianDay a) = ModJulianDay (pred a)
	toEnum = ModJulianDay . toEnum
	fromEnum (ModJulianDay a) = fromEnum a
	enumFrom (ModJulianDay a) = fmap ModJulianDay (enumFrom a)
	enumFromThen (ModJulianDay a) (ModJulianDay b) = fmap ModJulianDay (enumFromThen a b)
	enumFromTo (ModJulianDay a) (ModJulianDay b) = fmap ModJulianDay (enumFromTo a b)
	enumFromThenTo (ModJulianDay a) (ModJulianDay b) (ModJulianDay c) = fmap ModJulianDay (enumFromThenTo a b c)

addDate :: Date -> Integer -> Date
addDate (ModJulianDay a) b = ModJulianDay (a + b)

diffDate :: Date -> Date -> Integer
diffDate (ModJulianDay a) (ModJulianDay b) = a - b

{-
instance Show Date where
	show (ModJulianDay d) = "MJD " ++ (show d)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Num Date where
	(ModJulianDay a) + (ModJulianDay b) = ModJulianDay (a + b)
	(ModJulianDay a) - (ModJulianDay b) = ModJulianDay (a - b)
	(ModJulianDay a) * (ModJulianDay b) = ModJulianDay (a * b)
	negate (ModJulianDay a) = ModJulianDay (negate a)
	abs (ModJulianDay a) = ModJulianDay (abs a)
	signum (ModJulianDay a) = ModJulianDay (signum a)
	fromInteger = ModJulianDay

instance Real Date where
	toRational (ModJulianDay a) = toRational a

instance Integral Date where
	toInteger (ModJulianDay a) = toInteger a
	quotRem (ModJulianDay a) (ModJulianDay b) = (ModJulianDay c,ModJulianDay d) where
		(c,d) = quotRem a b
-}
