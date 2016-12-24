module Data.Time.Clock.System
(
    SystemTime(..),
    systemToUTCTime,getSystemTime,
    systemToTAITime,
    systemToPOSIXTime,
) where

import Data.Time.Clock.Internal.AbsoluteTime
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.GetTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.POSIXTime
import Data.Time.Calendar.Days
import Data.Int (Int64)


systemToUTCTime :: SystemTime -> UTCTime
systemToUTCTime (MkSystemTime s ns) = let
    (d, s') = s `divMod` 86400
    ps :: Int64
    ps = s' * 1000000000000 + (fromIntegral ns) * 1000
    in UTCTime (addDays (fromIntegral d) unixEpochDay) (picosecondsToDiffTime $ fromIntegral ps)

systemToPOSIXTime :: SystemTime -> POSIXTime
systemToPOSIXTime (MkSystemTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9

unixEpochAbsolute :: AbsoluteTime
unixEpochAbsolute = taiNominalDayStart unixEpochDay

systemToTAITime :: SystemTime -> AbsoluteTime
systemToTAITime (MkSystemTime s ns) = let
    diff :: DiffTime
    diff = (fromIntegral s) + (fromIntegral ns) * 1E-9
    in addAbsoluteTime diff unixEpochAbsolute

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587
