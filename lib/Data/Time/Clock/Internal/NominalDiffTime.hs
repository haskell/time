{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

module Data.Time.Clock.Internal.NominalDiffTime (
    NominalDiffTime,
    pattern Nominal,
    secondsToNominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
#ifdef __GLASGOW_HASKELL__
import GHC.Read
#endif
import Data.Time.Clock.Internal.DiffTime
import Language.Haskell.TH.Syntax qualified as TH
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

-- | This is a length of time, as measured by UTC.
-- It has a precision of one picosecond (10^-12 s).
--
-- Conversion functions such as 'fromInteger' and 'realToFrac' will treat it as seconds.
-- For example, @(0.010 :: NominalDiffTime)@ corresponds to 10 milliseconds.
--
-- Enumeration functions will treat it as picoseconds.
--
-- It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
-- For instance, 23:00 UTC + 2 hours of NominalDiffTime = 01:00 UTC (+ 1 day),
-- regardless of whether a leap-second intervened.
newtype NominalDiffTime
    = MkNominalDiffTime Pico
    deriving (Eq, Ord, Typeable, Data, TH.Lift)

-- | convert from DiffTime
pattern Nominal :: DiffTime -> NominalDiffTime
pattern Nominal dt <- MkNominalDiffTime (realToFrac -> dt)
    where
        Nominal dt = MkNominalDiffTime $ realToFrac dt

{-# COMPLETE Nominal #-}

-- | Create a 'NominalDiffTime' from a number of seconds.
--
-- @since 1.9.1
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = MkNominalDiffTime

-- | Get the seconds in a 'NominalDiffTime'.
--
-- @since 1.9.1
nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds (MkNominalDiffTime t) = t

instance NFData NominalDiffTime where
    rnf (MkNominalDiffTime t) = rnf t

instance Enum NominalDiffTime where
    succ (MkNominalDiffTime a) = MkNominalDiffTime (succ a)
    pred (MkNominalDiffTime a) = MkNominalDiffTime (pred a)
    toEnum = MkNominalDiffTime . toEnum
    fromEnum (MkNominalDiffTime a) = fromEnum a
    enumFrom (MkNominalDiffTime a) = fmap MkNominalDiffTime (enumFrom a)
    enumFromThen (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromThen a b)
    enumFromTo (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromTo a b)
    enumFromThenTo (MkNominalDiffTime a) (MkNominalDiffTime b) (MkNominalDiffTime c) =
        fmap MkNominalDiffTime (enumFromThenTo a b c)

instance Show NominalDiffTime where
    show (MkNominalDiffTime t) = (showFixed True t) ++ "s"

instance Read NominalDiffTime where
    readPrec = do
        t <- readPrec
        _ <- lift $ char 's'
        return $ MkNominalDiffTime t

instance Num NominalDiffTime where
    (MkNominalDiffTime a) + (MkNominalDiffTime b) = MkNominalDiffTime (a + b)
    (MkNominalDiffTime a) - (MkNominalDiffTime b) = MkNominalDiffTime (a - b)
    (MkNominalDiffTime a) * (MkNominalDiffTime b) = MkNominalDiffTime (a * b)
    negate (MkNominalDiffTime a) = MkNominalDiffTime (negate a)
    abs (MkNominalDiffTime a) = MkNominalDiffTime (abs a)
    signum (MkNominalDiffTime a) = MkNominalDiffTime (signum a)
    fromInteger i = MkNominalDiffTime (fromInteger i)

instance Real NominalDiffTime where
    toRational (MkNominalDiffTime a) = toRational a

instance Fractional NominalDiffTime where
    (MkNominalDiffTime a) / (MkNominalDiffTime b) = MkNominalDiffTime (a / b)
    recip (MkNominalDiffTime a) = MkNominalDiffTime (recip a)
    fromRational r = MkNominalDiffTime (fromRational r)

instance RealFrac NominalDiffTime where
    properFraction (MkNominalDiffTime a) = (i, MkNominalDiffTime f)
      where
        (i, f) = properFraction a
    truncate (MkNominalDiffTime a) = truncate a
    round (MkNominalDiffTime a) = round a
    ceiling (MkNominalDiffTime a) = ceiling a
    floor (MkNominalDiffTime a) = floor a

{-# RULES
"realToFrac/DiffTime->NominalDiffTime"
    realToFrac =
        \dt -> MkNominalDiffTime (realToFrac dt)
"realToFrac/NominalDiffTime->DiffTime"
    realToFrac =
        \(MkNominalDiffTime ps) -> realToFrac ps
"realToFrac/NominalDiffTime->Pico"
    realToFrac =
        \(MkNominalDiffTime ps) -> ps
"realToFrac/Pico->NominalDiffTime" realToFrac = MkNominalDiffTime
    #-}

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400
