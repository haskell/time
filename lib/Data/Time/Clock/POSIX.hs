-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    POSIXTime,
    makePOSIXTime,ptSeconds,ptNanoSeconds,
    posixSecondsToUTCTime,utcTimeToPOSIXSeconds,
    posixToUTCTime,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.GetTime
import Data.Time.Clock.UTC
import Data.Time.Clock.Scale (picosecondsToDiffTime)
import Data.Time.Calendar.Days
import Data.Int (Int64)
import Data.Word
import Data.Fixed (divMod')


makePOSIXTime :: Int64 -> Word32 -> POSIXTime
makePOSIXTime xs xn
    | xn < 0 || xn >= 1000000000 = POSIXTime (xs + fromIntegral q)  r
    | otherwise                  = POSIXTime xs xn
  where (q, r) = xn `divMod` 1000000000

posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime (POSIXTime s ns) = let
    (d, s') = s `divMod` 86400
    ps :: Int64
    ps = s' * 1000000000000 + (fromIntegral ns) * 1000
    in UTCTime (addDays (fromIntegral d) unixEpochDay) (picosecondsToDiffTime $ fromIntegral ps)

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

--------------------------------------------------------------------------------

posixSecondsToUTCTime :: NominalDiffTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i nominalDay
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> NominalDiffTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * nominalDay) + min nominalDay (realToFrac t)

-- | Get the current 'UTCTime' from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = posixToUTCTime `fmap` getPOSIXTime
