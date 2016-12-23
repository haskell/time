module Data.Time.Clock.Raw
(
    RawTime(..),
    rawToUTCTime,getRawTime,
    rawToTAITime,
    rawToPOSIXTime,
) where

import Data.Time.Clock.AbsoluteTime
import Data.Time.Clock.DiffTime
import Data.Time.Clock.GetTime
import Data.Time.Clock.UTC
import Data.Time.Clock.POSIXTime
import Data.Time.Calendar.Days
import Data.Int (Int64)


rawToUTCTime :: RawTime -> UTCTime
rawToUTCTime (MkRawTime s ns) = let
    (d, s') = s `divMod` 86400
    ps :: Int64
    ps = s' * 1000000000000 + (fromIntegral ns) * 1000
    in UTCTime (addDays (fromIntegral d) unixEpochDay) (picosecondsToDiffTime $ fromIntegral ps)

rawToPOSIXTime :: RawTime -> POSIXTime
rawToPOSIXTime (MkRawTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9

unixEpochAbsolute :: AbsoluteTime
unixEpochAbsolute = taiNominalDayStart unixEpochDay

rawToTAITime :: RawTime -> AbsoluteTime
rawToTAITime (MkRawTime s ns) = let
    diff :: DiffTime
    diff = (fromIntegral s) + (fromIntegral ns) * 1E-9
    in addAbsoluteTime diff unixEpochAbsolute

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587
