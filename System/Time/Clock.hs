{-# OPTIONS -ffi -Wall -Werror #-}

-- | Types and functions for UTC and UT1
module System.Time.Clock
(
	-- * Universal Time
	-- | Time as measured by the earth.
	ModJulianDay,ModJulianDate,

	-- * Absolute intervals
	DiffTime,

	-- * UTC
	-- | UTC is time as measured by a clock, corrected to keep pace with the earth by adding or removing
	-- occasional seconds, known as \"leap seconds\".
	-- These corrections are not predictable and are announced with six month's notice.
	-- No table of these corrections is provided, as any program compiled with it would become
	-- out of date in six months.
	UTCTime(..),UTCDiffTime,
	addUTCTime,diffUTCTime,

	-- * Current time
	getCurrentTime,
	
	-- * POSIX time
	-- | This is needed by System.Time.Calendar to talk to the Unix API.
	POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds
) where

import Data.Fixed

import Foreign
import Foreign.C

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
type ModJulianDay = Integer

-- | The Modified Julian Date is the day with the fraction of the day, measured from UT midnight.
-- It's used to represent UT1, which is time as measured by the earth's rotation, adjusted for various wobbles.
type ModJulianDate = Rational

-- | This is a length of time, as measured by a clock.
newtype DiffTime = MkDiffTime Pico deriving (Eq,Ord)

instance Enum DiffTime where
	succ (MkDiffTime a) = MkDiffTime (succ a)
	pred (MkDiffTime a) = MkDiffTime (pred a)
	toEnum = MkDiffTime . toEnum
	fromEnum (MkDiffTime a) = fromEnum a
	enumFrom (MkDiffTime a) = fmap MkDiffTime (enumFrom a)
	enumFromThen (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromThen a b)
	enumFromTo (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromTo a b)
	enumFromThenTo (MkDiffTime a) (MkDiffTime b) (MkDiffTime c) = fmap MkDiffTime (enumFromThenTo a b c)

instance Show DiffTime where
	show (MkDiffTime t) = (showFixed True t) ++ "s"

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Num DiffTime where
	(MkDiffTime a) + (MkDiffTime b) = MkDiffTime (a + b)
	(MkDiffTime a) - (MkDiffTime b) = MkDiffTime (a - b)
	(MkDiffTime a) * (MkDiffTime b) = MkDiffTime (a * b)
	negate (MkDiffTime a) = MkDiffTime (negate a)
	abs (MkDiffTime a) = MkDiffTime (abs a)
	signum (MkDiffTime a) = MkDiffTime (signum a)
	fromInteger i = MkDiffTime (fromInteger i)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Real DiffTime where
	toRational (MkDiffTime a) = toRational a

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Fractional DiffTime where
	(MkDiffTime a) / (MkDiffTime b) = MkDiffTime (a / b)
	recip (MkDiffTime a) = MkDiffTime (recip a)
	fromRational r = MkDiffTime (fromRational r)

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


-- Get current time

data CTimeval = MkCTimeval CLong CLong

ctimevalToPosixSeconds :: CTimeval -> POSIXTime
ctimevalToPosixSeconds (MkCTimeval s mus) = (fromIntegral s) + (fromIntegral mus) / 1000000

instance Storable CTimeval where
	sizeOf _ = (sizeOf (undefined :: CLong)) * 2
	alignment _ = alignment (undefined :: CLong)
	peek p = do
		s   <- peekElemOff (castPtr p) 0
		mus <- peekElemOff (castPtr p) 1
		return (MkCTimeval s mus)
	poke p (MkCTimeval s mus) = do
		pokeElemOff (castPtr p) 0 s
		pokeElemOff (castPtr p) 1 mus

foreign import ccall unsafe "time.h gettimeofday" gettimeofday :: Ptr CTimeval -> Ptr () -> IO CInt

-- | Get the current UTC time from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = with (MkCTimeval 0 0) (\ptval -> do
	result <- gettimeofday ptval nullPtr
	if (result == 0)
	 then do
	 	tval <- peek ptval
	 	return (posixSecondsToUTCTime (ctimevalToPosixSeconds tval))
	 else fail ("error in gettimeofday: " ++ (show result))
	)
