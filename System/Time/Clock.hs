{-# OPTIONS -ffi #-}

module System.Time.Clock
(
	-- Modified Julian days and dates (for UT1)
	ModJulianDay,ModJulianDate,

	-- absolute time intervals
	DiffTime,timeToSISeconds,siSecondsToTime,

	-- UTC arithmetic
	UTCTime(..),UTCDiffTime,utcTimeToUTCSeconds,utcSecondsToUTCTime,

	-- getting the current UTC time
	getCurrentTime
) where

import Foreign
import Foreign.C

-- | standard Julian count of Earth days
type ModJulianDay = Integer

-- | standard Julian dates for UT1, 1 = 1 day
type ModJulianDate = Rational

secondPicoseconds :: (Num a) => a
secondPicoseconds = 1000000000000

newtype DiffTime = MkDiffTime Integer deriving (Eq,Ord,Show)

timeToSIPicoseconds :: DiffTime -> Integer
timeToSIPicoseconds (MkDiffTime ps) = ps

siPicosecondsToTime :: Integer -> DiffTime
siPicosecondsToTime = MkDiffTime

timeToSISeconds :: (Fractional a) => DiffTime -> a
timeToSISeconds t = fromRational ((toRational (timeToSIPicoseconds t)) / (toRational secondPicoseconds));

siSecondsToTime :: (Real a) => a -> DiffTime
siSecondsToTime t = siPicosecondsToTime (round ((toRational t) * secondPicoseconds))

data UTCTime = UTCTime {
	utctDay :: ModJulianDay,
	utctDayTime :: DiffTime
}

newtype UTCDiffTime = MkUTCDiffTime Integer

utcTimeToUTCPicoseconds :: UTCDiffTime -> Integer
utcTimeToUTCPicoseconds (MkUTCDiffTime ps) = ps

utcPicosecondsToUTCTime :: Integer -> UTCDiffTime
utcPicosecondsToUTCTime = MkUTCDiffTime

utcTimeToUTCSeconds :: (Fractional a) => UTCDiffTime -> a
utcTimeToUTCSeconds t = fromRational ((toRational (utcTimeToUTCPicoseconds t)) / (toRational secondPicoseconds))

utcSecondsToUTCTime :: (Real a) => a -> UTCDiffTime
utcSecondsToUTCTime t = utcPicosecondsToUTCTime (round ((toRational t) * secondPicoseconds))

posixDaySeconds :: (Num a) => a
posixDaySeconds = 86400

posixDayPicoseconds :: Integer
posixDayPicoseconds = posixDaySeconds * secondPicoseconds

unixEpochMJD :: ModJulianDay
unixEpochMJD = 40587

posixPicosecondsToUTCTime :: Integer -> UTCTime
posixPicosecondsToUTCTime i = let
	(d,t) = divMod i posixDayPicoseconds
 in UTCTime (d + unixEpochMJD) (siPicosecondsToTime t)

utcTimeToPOSIXPicoseconds :: UTCTime -> Integer
utcTimeToPOSIXPicoseconds (UTCTime d t) =
 ((d - unixEpochMJD) * posixDayPicoseconds) + min posixDayPicoseconds (timeToSIPicoseconds t)

addUTCTime :: UTCDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixPicosecondsToUTCTime ((utcTimeToUTCPicoseconds x) + (utcTimeToPOSIXPicoseconds t))

diffUTCTime :: UTCTime -> UTCTime -> UTCDiffTime
diffUTCTime a b = utcPicosecondsToUTCTime ((utcTimeToPOSIXPicoseconds a) - (utcTimeToPOSIXPicoseconds b))


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

getCurrentTime :: IO UTCTime
getCurrentTime = with (MkCTimeval 0 0) (\ptval -> do
	result <- gettimeofday ptval nullPtr
	if (result == 0)
	 then do
	 	tval <- peek ptval
	 	return (posixPicosecondsToUTCTime (ctimevalToPosixPicoseconds tval))
	 else fail ("error in gettimeofday: " ++ (show result))
	)
