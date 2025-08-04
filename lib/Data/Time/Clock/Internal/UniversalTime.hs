{-# LANGUAGE Safe #-}

module Data.Time.Clock.Internal.UniversalTime where

import Control.DeepSeq
import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH

-- | The Modified Julian Date is the day with the fraction of the day, measured from UT midnight.
-- It's used to represent UT1, which is time as measured by the earth's rotation, adjusted for various wobbles.
newtype UniversalTime = ModJulianDate
    { getModJulianDate :: Rational
    }
    deriving (Eq, Ord, Typeable, Data, Generic, TH.Lift)

instance NFData UniversalTime where
    rnf (ModJulianDate a) = rnf a
