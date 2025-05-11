{-# LANGUAGE Safe #-}

module Data.Time.Clock.Internal.UTCTime (
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
) where

import Control.DeepSeq
import Data.Data
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.DiffTime
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH

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
