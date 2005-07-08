{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Clock.UTC
(
	-- * UTC
	-- | UTC is time as measured by a clock, corrected to keep pace with the earth by adding or removing
	-- occasional seconds, known as \"leap seconds\".
	-- These corrections are not predictable and are announced with six month's notice.
	-- No table of these corrections is provided, as any program compiled with it would become
	-- out of date in six months.
	UTCTime(..),UTCDiffTime,
	addUTCTime,diffUTCTime,
	
	-- * POSIX time
	-- | This is needed by System.Time.Calendar to talk to the Unix API.
	posixDay,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds
) where

import System.Time.Clock.Scale
import Data.Fixed

-- | This is the simplest representation of UTC.
-- It consists of the day number, and a time offset from midnight.
-- Note that if a day has a leap second added to it, it will have 86401 seconds.
data UTCTime = UTCTime {
	-- | the day
	utctDay :: ModJulianDay,
	-- | the time from midnight, 0 <= t < 86401s (because of leap-seconds)
	utctDayTime :: DiffTime
}

instance Eq UTCTime where
	(UTCTime da ta) == (UTCTime db tb) = (da == db) && (ta == tb)

instance Ord UTCTime where
	compare (UTCTime da ta) (UTCTime db tb) = case (compare da db) of
		EQ -> compare ta tb
		cmp -> cmp

-- | This is a length of time, as measured by UTC.
-- Conversion functions will treat it as seconds.
-- It has an accuracy of 10^-12 s.
-- It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
-- For instance, 23:00 UTC + 2 hours of UTCDiffTime = 01:00 UTC (+ 1 day),
-- regardless of whether a leap-second intervened.
newtype UTCDiffTime = MkUTCDiffTime Pico deriving (Eq,Ord)

instance Enum UTCDiffTime where
	succ (MkUTCDiffTime a) = MkUTCDiffTime (succ a)
	pred (MkUTCDiffTime a) = MkUTCDiffTime (pred a)
	toEnum = MkUTCDiffTime . toEnum
	fromEnum (MkUTCDiffTime a) = fromEnum a
	enumFrom (MkUTCDiffTime a) = fmap MkUTCDiffTime (enumFrom a)
	enumFromThen (MkUTCDiffTime a) (MkUTCDiffTime b) = fmap MkUTCDiffTime (enumFromThen a b)
	enumFromTo (MkUTCDiffTime a) (MkUTCDiffTime b) = fmap MkUTCDiffTime (enumFromTo a b)
	enumFromThenTo (MkUTCDiffTime a) (MkUTCDiffTime b) (MkUTCDiffTime c) = fmap MkUTCDiffTime (enumFromThenTo a b c)

instance Show UTCDiffTime where
	show (MkUTCDiffTime t) = (showFixed True t) ++ "s"

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Num UTCDiffTime where
	(MkUTCDiffTime a) + (MkUTCDiffTime b) = MkUTCDiffTime (a + b)
	(MkUTCDiffTime a) - (MkUTCDiffTime b) = MkUTCDiffTime (a - b)
	(MkUTCDiffTime a) * (MkUTCDiffTime b) = MkUTCDiffTime (a * b)
	negate (MkUTCDiffTime a) = MkUTCDiffTime (negate a)
	abs (MkUTCDiffTime a) = MkUTCDiffTime (abs a)
	signum (MkUTCDiffTime a) = MkUTCDiffTime (signum a)
	fromInteger i = MkUTCDiffTime (fromInteger i)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Real UTCDiffTime where
	toRational (MkUTCDiffTime a) = toRational a

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Fractional UTCDiffTime where
	(MkUTCDiffTime a) / (MkUTCDiffTime b) = MkUTCDiffTime (a / b)
	recip (MkUTCDiffTime a) = MkUTCDiffTime (recip a)
	fromRational r = MkUTCDiffTime (fromRational r)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance RealFrac UTCDiffTime where
	properFraction (MkUTCDiffTime a) = (i,MkUTCDiffTime f) where
		(i,f) = properFraction a
	truncate (MkUTCDiffTime a) = truncate a
	round (MkUTCDiffTime a) = round a
	ceiling (MkUTCDiffTime a) = ceiling a
	floor (MkUTCDiffTime a) = floor a

posixDay :: UTCDiffTime
posixDay = 86400

unixEpochMJD :: ModJulianDay
unixEpochMJD = 40587

type POSIXTime = UTCDiffTime

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime i = let
	(d,t) = divMod' i posixDay
 in UTCTime (d + unixEpochMJD) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (d - unixEpochMJD) * posixDay) + min posixDay (realToFrac t)

-- | addUTCTime a b = a + b
addUTCTime :: UTCDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime (x + (utcTimeToPOSIXSeconds t))

-- | diffUTCTime a b = a - b
diffUTCTime :: UTCTime -> UTCTime -> UTCDiffTime
diffUTCTime a b = (utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b)
