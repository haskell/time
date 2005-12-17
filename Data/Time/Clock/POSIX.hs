{-# OPTIONS -Wall -Werror #-}

-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
	posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime
) where

import Data.Time.Clock.CTimeval
import Data.Time.Clock.UTC
import Data.Time.Calendar.Days
import Data.Fixed

import Control.Monad

ctimevalToPosixSeconds :: CTimeval -> POSIXTime
ctimevalToPosixSeconds (MkCTimeval s mus) = (fromIntegral s) + (fromIntegral mus) / 1000000

-- | 86400 nominal seconds in every day
posixDayLength :: NominalDiffTime
posixDayLength = 86400

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
type POSIXTime = NominalDiffTime

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime i = let
	(d,t) = divMod' i posixDayLength
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * posixDayLength) + min posixDayLength (realToFrac t)

-- | Get the current POSIX time from the system clock.
getPOSIXTime :: IO POSIXTime
getPOSIXTime = liftM ctimevalToPosixSeconds getCTimeval
