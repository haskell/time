{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Clock
(
	-- Modified Julian days and dates (for UT1)
	ModJulianDay,ModJulianDate,

	-- absolute time intervals
	DiffTime,

	-- UTC arithmetic
	UTCTime(..),UTCDiffTime,
	addUTCTime,diffUTCTime,

	-- getting the current UTC time
	getCurrentTime
) where

import Data.Fixed

import Foreign
import Foreign.C

-- | standard Modified Julian Day, a count of Earth days
type ModJulianDay = Integer

-- | standard Modified Julian Date to represent UT1, 1 = 1 day
type ModJulianDate = Rational

-- | a length of time
newtype DiffTime = MkDiffTime Pico deriving (Eq,Ord,Enum)

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

-- | time in UTC
data UTCTime = UTCTime {
	-- | the day
	utctDay :: ModJulianDay,
	-- | the time from midnight, 0 <= t < 61s (because of leap-seconds)
	utctDayTime :: DiffTime
}

-- | a length of time for UTC, ignoring leap-seconds
newtype UTCDiffTime = MkUTCDiffTime Pico deriving (Eq,Ord,Enum)

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

posixDaySeconds :: Pico
posixDaySeconds = 86400

unixEpochMJD :: ModJulianDay
unixEpochMJD = 40587


posixSecondsToUTCTime :: Pico -> UTCTime
posixSecondsToUTCTime i = let
	(d,t) = divMod' i posixDaySeconds
 in UTCTime (d + unixEpochMJD) (fromReal t)

utcTimeToPOSIXSeconds :: UTCTime -> Pico
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (d - unixEpochMJD) * posixDaySeconds) + min posixDaySeconds (fromReal t)


addUTCTime :: UTCDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime ((fromReal x) + (utcTimeToPOSIXSeconds t))

diffUTCTime :: UTCTime -> UTCTime -> UTCDiffTime
diffUTCTime a b = fromReal ((utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b))


-- Get current time

data CTimeval = MkCTimeval CLong CLong

ctimevalToPosixSeconds :: CTimeval -> Pico
ctimevalToPosixSeconds (MkCTimeval s mus) = ((fromIntegral s) + (fromIntegral mus) / 1000000)

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

foreign import ccall unsafe "sys/time.h gettimeofday" gettimeofday :: Ptr CTimeval -> Ptr () -> IO CInt

-- | get the current time
getCurrentTime :: IO UTCTime
getCurrentTime = with (MkCTimeval 0 0) (\ptval -> do
	result <- gettimeofday ptval nullPtr
	if (result == 0)
	 then do
	 	tval <- peek ptval
	 	return (posixSecondsToUTCTime (ctimevalToPosixSeconds tval))
	 else fail ("error in gettimeofday: " ++ (show result))
	)
