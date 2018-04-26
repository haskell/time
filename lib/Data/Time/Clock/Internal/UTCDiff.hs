module Data.Time.Clock.Internal.UTCDiff where

import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX

-- | addUTCTime a b = a + b
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime (x + (utcTimeToPOSIXSeconds t))

-- | subractUTCTime a b = b - a
subractUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subractUTCTime x t
  | utcTimeToPOSIXSeconds t >= x = posixSecondsToUTCTime (utcTimeToPOSIXSeconds t - x)
  | otherwise                    = posixSecondsToUTCTime 0

-- | diffUTCTime a b = a - b
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b = (utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b)
