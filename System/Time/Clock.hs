{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Clock
(
	-- Modified Julian days and dates (for UT1)
	ModJulianDay,ModJulianDate,

	-- absolute time intervals
	DiffTime,siSecond,timeToSISeconds,siSecondsToTime,

	-- UTC arithmetic
	UTCTime(..),UTCDiffTime,utcTimeToUTCSeconds,utcSecondsToUTCTime,
	addUTCTime,diffUTCTime,

	-- getting the current UTC time
	getCurrentTime
) where

import Foreign
import Foreign.C

-- | standard Modified Julian Day, a count of Earth days
type ModJulianDay = Integer

-- | standard Modified Julian Date to represent UT1, 1 = 1 day
type ModJulianDate = Rational

-- | the number of picoseconds in a second
secondPicoseconds :: (Num a) => a
secondPicoseconds = 1000000000000

-- | a length of time
newtype DiffTime = MkDiffTime Integer deriving (Eq,Ord,Enum)

instance Show DiffTime where
	show (MkDiffTime t) = (show t) ++ "ps"

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
instance Integral DiffTime where
	quot (MkDiffTime a) (MkDiffTime b) = MkDiffTime (quot a b)
	rem (MkDiffTime a) (MkDiffTime b) = MkDiffTime (rem a b)
	div (MkDiffTime a) (MkDiffTime b) = MkDiffTime (div a b)
	mod (MkDiffTime a) (MkDiffTime b) = MkDiffTime (mod a b)
	quotRem (MkDiffTime a) (MkDiffTime b) = (MkDiffTime p,MkDiffTime q) where
		(p,q) = quotRem a b
	divMod (MkDiffTime a) (MkDiffTime b) = (MkDiffTime p,MkDiffTime q) where
		(p,q) = divMod a b
	toInteger (MkDiffTime a) = toInteger a

siSecond :: DiffTime
siSecond = secondPicoseconds

timeToSISeconds :: (Fractional a) => DiffTime -> a
timeToSISeconds t = fromRational ((toRational t) / secondPicoseconds);

siSecondsToTime :: (Real a) => a -> DiffTime
siSecondsToTime t = fromInteger (round ((toRational t) * secondPicoseconds))

-- | time in UTC
data UTCTime = UTCTime {
	-- | the day
	utctDay :: ModJulianDay,
	-- | the time from midnight, 0 <= t < 61s (because of leap-seconds)
	utctDayTime :: DiffTime
}

-- | a length of time for UTC, ignoring leap-seconds
newtype UTCDiffTime = MkUTCDiffTime Integer deriving (Eq,Ord,Enum)

instance Show UTCDiffTime where
	show (MkUTCDiffTime t) = (show t) ++ "ps"

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
instance Integral UTCDiffTime where
	quot (MkUTCDiffTime a) (MkUTCDiffTime b) = MkUTCDiffTime (quot a b)
	rem (MkUTCDiffTime a) (MkUTCDiffTime b) = MkUTCDiffTime (rem a b)
	div (MkUTCDiffTime a) (MkUTCDiffTime b) = MkUTCDiffTime (div a b)
	mod (MkUTCDiffTime a) (MkUTCDiffTime b) = MkUTCDiffTime (mod a b)
	quotRem (MkUTCDiffTime a) (MkUTCDiffTime b) = (MkUTCDiffTime p,MkUTCDiffTime q) where
		(p,q) = quotRem a b
	divMod (MkUTCDiffTime a) (MkUTCDiffTime b) = (MkUTCDiffTime p,MkUTCDiffTime q) where
		(p,q) = divMod a b
	toInteger (MkUTCDiffTime a) = toInteger a

utcTimeToUTCSeconds :: (Fractional a) => UTCDiffTime -> a
utcTimeToUTCSeconds t = fromRational ((toRational t) / secondPicoseconds)

utcSecondsToUTCTime :: (Real a) => a -> UTCDiffTime
utcSecondsToUTCTime t = fromInteger (round ((toRational t) * secondPicoseconds))

posixDaySeconds :: (Num a) => a
posixDaySeconds = 86400

posixDayPicoseconds :: Integer
posixDayPicoseconds = posixDaySeconds * secondPicoseconds

unixEpochMJD :: ModJulianDay
unixEpochMJD = 40587

posixPicosecondsToUTCTime :: Integer -> UTCTime
posixPicosecondsToUTCTime i = let
	(d,t) = divMod i posixDayPicoseconds
 in UTCTime (d + unixEpochMJD) (fromInteger t)

utcTimeToPOSIXPicoseconds :: UTCTime -> Integer
utcTimeToPOSIXPicoseconds (UTCTime d t) =
 ((d - unixEpochMJD) * posixDayPicoseconds) + min posixDayPicoseconds (toInteger t)

addUTCTime :: UTCDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixPicosecondsToUTCTime ((toInteger x) + (utcTimeToPOSIXPicoseconds t))

diffUTCTime :: UTCTime -> UTCTime -> UTCDiffTime
diffUTCTime a b = fromInteger ((utcTimeToPOSIXPicoseconds a) - (utcTimeToPOSIXPicoseconds b))


-- Get current time

data CTimeval = MkCTimeval CLong CLong

ctimevalToPosixPicoseconds :: CTimeval -> Integer
ctimevalToPosixPicoseconds (MkCTimeval s mus) = ((fromIntegral s) * 1000000 + (fromIntegral mus)) * 1000000

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
	 	return (posixPicosecondsToUTCTime (ctimevalToPosixPicoseconds tval))
	 else fail ("error in gettimeofday: " ++ (show result))
	)
