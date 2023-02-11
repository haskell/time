{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Time.Clock.Internal.NominalDiffTime (
    NominalDiffTime,
    secondsToNominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import GHC.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import qualified Language.Haskell.TH.Syntax as TH

-- | This is a length of time, as measured by UTC.
-- It has a precision of 10^-12 s.
--
-- Conversion functions such as 'fromInteger' and 'realToFrac' will treat it as seconds.
-- For example, @(0.010 :: NominalDiffTime)@ corresponds to 10 milliseconds.
--
-- It has a precision of one picosecond (= 10^-12 s). Enumeration functions will treat it as picoseconds.
--
-- It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
-- For instance, 23:00 UTC + 2 hours of NominalDiffTime = 01:00 UTC (+ 1 day),
-- regardless of whether a leap-second intervened.
newtype NominalDiffTime
    = MkNominalDiffTime Pico
    deriving (Eq, Ord, Data, Typeable)

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

-- Let GHC derive the instances when 'Fixed' has 'TH.Lift' instance.
instance TH.Lift NominalDiffTime where
    liftTyped :: TH.Quote m => NominalDiffTime -> TH.Code m NominalDiffTime
    liftTyped (MkNominalDiffTime (MkFixed a)) = [||MkNominalDiffTime (MkFixed $$(TH.liftTyped a))||]

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
