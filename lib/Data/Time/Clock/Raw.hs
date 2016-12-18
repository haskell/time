-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.Raw
(
    RawTime(..),
    rawToUTCTime,getRawTime,getCurrentTime
) where

import Data.Time.Clock.GetTime
import Data.Time.Clock.UTC
import Data.Time.Clock.Scale (picosecondsToDiffTime)
import Data.Time.Calendar.Days
import Data.Int (Int64)


rawToUTCTime :: RawTime -> UTCTime
rawToUTCTime (MkRawTime s ns) = let
    (d, s') = s `divMod` 86400
    ps :: Int64
    ps = s' * 1000000000000 + (fromIntegral ns) * 1000
    in UTCTime (addDays (fromIntegral d) unixEpochDay) (picosecondsToDiffTime $ fromIntegral ps)

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587


-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = rawToUTCTime `fmap` getRawTime
