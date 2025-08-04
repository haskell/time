{-# LANGUAGE Safe #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.LocalTime.Internal.ZonedTime where

import Control.DeepSeq
import Data.Data
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX
import Data.Time.LocalTime.Internal.Foreign
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeZone
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH

-- | A local time together with a time zone.
--
-- There is no 'Eq' instance for @ZonedTime@.
-- If you want to compare local times, use 'zonedTimeToLocalTime'.
-- If you want to compare absolute times, use 'zonedTimeToUTC'.
data ZonedTime = ZonedTime
    { zonedTimeToLocalTime :: LocalTime
    , zonedTimeZone :: TimeZone
    }
    deriving (Typeable, Data, Generic, TH.Lift)

instance NFData ZonedTime where
    rnf (ZonedTime lt z) = rnf lt `seq` rnf z `seq` ()

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime zone time = ZonedTime (utcToLocalTime zone time) zone

zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC (ZonedTime t zone) = localTimeToUTC zone t

-- | For the time zone, this only shows the name, or offset if the name is empty.
instance Show ZonedTime where
    show (ZonedTime t zone) = show t ++ " " ++ show zone

-- orphan instance
instance Show UTCTime where
    show t = show (utcToZonedTime utc t)

getZonedTime :: IO ZonedTime
getZonedTime = do
    t <- getCurrentTime
    zone <- getTimeZone t
    return (utcToZonedTime zone t)

utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime t = do
    zone <- getTimeZone t
    return (utcToZonedTime zone t)
