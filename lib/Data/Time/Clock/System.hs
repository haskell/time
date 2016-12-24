module Data.Time.Clock.System
(
    unixEpochDay,
    SystemTime(..),
    truncateSystemTimeLeapSecond,
    getSystemTime,
    systemToUTCTime,
    utcToSystemTime,
    systemToTAITime,
) where

import Data.Time.Clock.Internal.AbsoluteTime
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.GetTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Calendar.Days
import Data.Int (Int64)


truncateSystemTimeLeapSecond :: SystemTime -> SystemTime
truncateSystemTimeLeapSecond (MkSystemTime seconds nanoseconds) | nanoseconds >= 1000000000 = MkSystemTime (succ seconds) 0
truncateSystemTimeLeapSecond t = t

systemToUTCTime :: SystemTime -> UTCTime
systemToUTCTime (MkSystemTime seconds nanoseconds) = let
    days :: Int64
    timeSeconds :: Int64
    (days, timeSeconds) = seconds `divMod` 86400

    day :: Day
    day = addDays (fromIntegral days) unixEpochDay

    timeNanoseconds :: Int64
    timeNanoseconds = timeSeconds * 1000000000 + (fromIntegral nanoseconds)

    timePicoseconds :: Int64
    timePicoseconds = timeNanoseconds * 1000

    time :: DiffTime
    time = picosecondsToDiffTime $ fromIntegral timePicoseconds
    in UTCTime day time

utcToSystemTime :: UTCTime -> SystemTime
utcToSystemTime (UTCTime day time) = let
    days :: Int64
    days = fromIntegral $ diffDays day unixEpochDay

    timePicoseconds :: Int64
    timePicoseconds = fromIntegral $ diffTimeToPicoseconds time

    timeNanoseconds :: Int64
    timeNanoseconds = timePicoseconds `div` 1000

    timeSeconds :: Int64
    nanoseconds :: Int64
    (timeSeconds,nanoseconds) = if timeNanoseconds >= 86400000000000 then (86399,timeNanoseconds - 86399000000000) else timeNanoseconds `divMod` 1000000000

    seconds :: Int64
    seconds = days * 86400 + timeSeconds

    in MkSystemTime seconds $ fromIntegral nanoseconds

unixEpochAbsolute :: AbsoluteTime
unixEpochAbsolute = taiNominalDayStart unixEpochDay

systemToTAITime :: SystemTime -> AbsoluteTime
systemToTAITime (MkSystemTime s ns) = let
    diff :: DiffTime
    diff = (fromIntegral s) + (fromIntegral ns) * 1E-9
    in addAbsoluteTime diff unixEpochAbsolute

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587
