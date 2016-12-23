-- | Types and functions for UTC and UT1
module Data.Time.Clock
(
    module Data.Time.Clock.UniversalTime,
    module Data.Time.Clock.DiffTime,
    module Data.Time.Clock.UTC,
    module Data.Time.Clock.NominalDiffTime,
    module Data.Time.Clock.UTCDiff,
    getCurrentTime,
    getTime_resolution
) where

import Data.Time.Clock.UniversalTime
import Data.Time.Clock.DiffTime
import Data.Time.Clock.GetTime
import Data.Time.Clock.UTCDiff
import Data.Time.Clock.NominalDiffTime
import Data.Time.Clock.UTC
import Data.Time.Clock.POSIX
import Data.Time.Format.Parse()
import Data.Time.LocalTime()
