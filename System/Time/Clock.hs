{-# OPTIONS -ffi -fglasgow-exts #-}

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
newtype DiffTime = MkDiffTime Integer deriving (Eq,Ord,Num,Enum,Real,Integral)

instance Show DiffTime where
	show (MkDiffTime t) = (show t) ++ "ps"

siSecond :: DiffTime
siSecond = secondPicoseconds

timeToSISeconds :: (Fractional a) => DiffTime -> a
timeToSISeconds t = fromRational ((toRational t) / (toRational secondPicoseconds));

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
newtype UTCDiffTime = MkUTCDiffTime Integer deriving (Eq,Ord,Num,Enum,Real,Integral)

instance Show UTCDiffTime where
	show (MkUTCDiffTime t) = (show t) ++ "ps"

utcTimeToUTCSeconds :: (Fractional a) => UTCDiffTime -> a
utcTimeToUTCSeconds t = fromRational ((toRational t) / (toRational secondPicoseconds))

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
