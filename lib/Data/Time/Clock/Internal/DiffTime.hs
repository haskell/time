{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Data.Time.Clock.Internal.DiffTime (
    -- * Absolute intervals
    DiffTime,
    secondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToPicoseconds,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import GHC.Read
import qualified Language.Haskell.TH.Syntax as TH
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

-- | This is a length of time, as measured by a clock.
-- Conversion functions such as 'fromInteger' and 'realToFrac' will treat it as seconds.
-- For example, @(0.010 :: DiffTime)@ corresponds to 10 milliseconds.
--
-- It has a precision of one picosecond (= 10^-12 s). Enumeration functions will treat it as picoseconds.
newtype DiffTime
    = MkDiffTime Pico
    deriving (Eq, Ord, Data, Typeable)

instance NFData DiffTime where
    rnf (MkDiffTime t) = rnf t

instance Enum DiffTime where
    succ (MkDiffTime a) = MkDiffTime (succ a)
    pred (MkDiffTime a) = MkDiffTime (pred a)
    toEnum = MkDiffTime . toEnum
    fromEnum (MkDiffTime a) = fromEnum a
    enumFrom (MkDiffTime a) = fmap MkDiffTime (enumFrom a)
    enumFromThen (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromThen a b)
    enumFromTo (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromTo a b)
    enumFromThenTo (MkDiffTime a) (MkDiffTime b) (MkDiffTime c) = fmap MkDiffTime (enumFromThenTo a b c)

instance Show DiffTime where
    show (MkDiffTime t) = (showFixed True t) ++ "s"

instance Read DiffTime where
    readPrec = do
        t <- readPrec
        _ <- lift $ char 's'
        return $ MkDiffTime t

instance Num DiffTime where
    (MkDiffTime a) + (MkDiffTime b) = MkDiffTime (a + b)
    (MkDiffTime a) - (MkDiffTime b) = MkDiffTime (a - b)
    (MkDiffTime a) * (MkDiffTime b) = MkDiffTime (a * b)
    negate (MkDiffTime a) = MkDiffTime (negate a)
    abs (MkDiffTime a) = MkDiffTime (abs a)
    signum (MkDiffTime a) = MkDiffTime (signum a)
    fromInteger i = MkDiffTime (fromInteger i)

instance Real DiffTime where
    toRational (MkDiffTime a) = toRational a

instance Fractional DiffTime where
    (MkDiffTime a) / (MkDiffTime b) = MkDiffTime (a / b)
    recip (MkDiffTime a) = MkDiffTime (recip a)
    fromRational r = MkDiffTime (fromRational r)

instance RealFrac DiffTime where
    properFraction (MkDiffTime a) =
        let
            (b', a') = properFraction a
        in
            (b', MkDiffTime a')
    truncate (MkDiffTime a) = truncate a
    round (MkDiffTime a) = round a
    ceiling (MkDiffTime a) = ceiling a
    floor (MkDiffTime a) = floor a

-- Let GHC derive the instances when 'Fixed' has 'TH.Lift' instance.
instance TH.Lift DiffTime where
    liftTyped :: TH.Quote m => DiffTime -> TH.Code m DiffTime
    liftTyped (MkDiffTime (MkFixed a)) = [||MkDiffTime (MkFixed $$(TH.liftTyped a))||]

-- | Create a 'DiffTime' which represents an integral number of seconds.
secondsToDiffTime :: Integer -> DiffTime
secondsToDiffTime = fromInteger

-- | Create a 'DiffTime' from a number of picoseconds.
picosecondsToDiffTime :: Integer -> DiffTime
picosecondsToDiffTime x = MkDiffTime (MkFixed x)

-- | Get the number of picoseconds in a 'DiffTime'.
diffTimeToPicoseconds :: DiffTime -> Integer
diffTimeToPicoseconds (MkDiffTime (MkFixed x)) = x

{-# RULES
"realToFrac/DiffTime->Pico" realToFrac = \(MkDiffTime ps) -> ps
"realToFrac/Pico->DiffTime" realToFrac = MkDiffTime
    #-}
