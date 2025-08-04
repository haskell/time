{-# LANGUAGE Safe #-}

-- | Types and functions for UTC and UT1
module Data.Time.Clock (
    -- * Universal Time

    -- | Time as measured by the Earth.
    UniversalTime (..),

    -- * Absolute intervals
    DiffTime,
    pattern Picoseconds,
    pattern Seconds,
    pattern Minutes,
    pattern Hours,
    secondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToPicoseconds,

    -- * UTC

    -- | UTC is time as measured by a clock, corrected to keep pace with the earth by adding or removing
    -- occasional seconds, known as \"leap seconds\".
    -- These corrections are not predictable and are announced with six month's notice.
    -- No table of these corrections is provided, as any program compiled with it would become
    -- out of date in six months.
    --
    -- If you don't care about leap seconds, use 'UTCTime' and 'NominalDiffTime' for your clock calculations,
    -- and you'll be fine.
    UTCTime (..),
    NominalDiffTime,
    pattern Nominal,
    secondsToNominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay,
    module Data.Time.Clock.Internal.UTCDiff,
    getCurrentTime,
    getTime_resolution,
) where

import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.SystemTime
import Data.Time.Clock.Internal.UTCDiff
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.POSIX
import Data.Time.Format.Parse ()
import Data.Time.LocalTime ()
