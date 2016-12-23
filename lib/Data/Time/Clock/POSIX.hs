-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.GetTime
import Data.Time.Clock.POSIXTime
import Data.Time.Clock.UTC
import Data.Time.Clock.Raw
import Data.Time.Calendar.Days
import Data.Fixed

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
getPOSIXTime = fmap rawToPOSIXTime getRawTime

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = rawToUTCTime `fmap` getRawTime
