-- | Types and functions for UTC and UT1
module Data.Time.Clock
(
    module Data.Time.Clock.Scale,
    module Data.Time.Clock.UTC,
    module Data.Time.Clock.UTCDiff,
    getCurrentTime,
    clockResolution
) where

import Data.Time.Clock.Scale
import Data.Time.Clock.GetTime
import Data.Time.Clock.UTCDiff
import Data.Time.Clock.UTC
import Data.Time.Clock.POSIX
import Data.Time.Format.Parse()
import Data.Time.LocalTime()
