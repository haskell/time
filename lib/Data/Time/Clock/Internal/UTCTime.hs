{-# LANGUAGE Safe #-}

module Data.Time.Clock.Internal.UTCTime where

import Control.DeepSeq
import Data.Data
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.DiffTime
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH

-- | This is the simplest representation of UTC.
-- It consists of the day number, and a time offset from midnight.
-- Note that if a day has a leap second added to it, it will have 86401 seconds.
data UTCTime = UTCTime
    { utctDay :: Day
    -- ^ the day
    , utctDayTime :: DiffTime
    -- ^ the time from midnight, 0 <= t < 86401s (because of leap-seconds)
    }
    deriving (Eq, Ord, Typeable, Data, Generic, TH.Lift)

instance NFData UTCTime where
    rnf (UTCTime d t) = rnf d `seq` rnf t `seq` ()
