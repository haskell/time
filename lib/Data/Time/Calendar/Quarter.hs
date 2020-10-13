{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}
#endif

-- | Year quarters.
module Data.Time.Calendar.Quarter
    (
        QuarterOfYear(..), addQuarters, diffQuarters,
        Quarter(..),
        pattern YearQuarter,
        monthOfYearQuarter,
        monthQuarter,
        dayQuarter
    ) where

import Data.Time.Calendar.Types
import Data.Time.Calendar.Private
import Data.Time.Calendar.Days
import Data.Time.Calendar.Month
import Data.Data
import Data.Fixed
import Text.Read
import Text.ParserCombinators.ReadP

-- | Quarters of each year. Each quarter corresponds to three months.
data QuarterOfYear = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Data, Typeable, Read, Show)

-- | maps Q1..Q4 to 1..4
instance Enum QuarterOfYear where
    toEnum i =
        case mod' i 4 of
            1 -> Q1
            2 -> Q2
            3 -> Q3
            _ -> Q4
    fromEnum Q1 = 1
    fromEnum Q2 = 2
    fromEnum Q3 = 3
    fromEnum Q4 = 4

instance Bounded QuarterOfYear where
    minBound = Q1
    maxBound = Q4

-- | An absolute count of year quarters.
-- Number is equal to @(year * 4) + (quarterOfYear - 1)@.
newtype Quarter = MkQuarter Integer deriving (Eq, Ord, Data, Typeable)

-- | Show as @yyyy-Qn@.
instance Show Quarter where
    show (YearQuarter y qy) = show4 y ++ "-" ++ show qy

-- | Read as @yyyy-Qn@.
instance Read Quarter where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ YearQuarter y m

addQuarters :: Integer -> Quarter -> Quarter
addQuarters n (MkQuarter a) = MkQuarter $ a + n

diffQuarters :: Quarter -> Quarter -> Integer
diffQuarters (MkQuarter a) (MkQuarter b) = a - b

-- | Bidirectional abstract constructor.
pattern YearQuarter :: Year -> QuarterOfYear -> Quarter
pattern YearQuarter y qy <- MkQuarter ((\q -> divMod' q 4) -> (y,toEnum . succ . fromInteger -> qy)) where
    YearQuarter y qy = MkQuarter $ (y * 4) + toInteger (pred $ fromEnum qy)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearQuarter #-}
#endif

monthOfYearQuarter :: MonthOfYear -> QuarterOfYear
monthOfYearQuarter my | my <= 3 = Q1
monthOfYearQuarter my | my <= 6 = Q2
monthOfYearQuarter my | my <= 9 = Q3
monthOfYearQuarter _ = Q4

monthQuarter :: Month -> Quarter
monthQuarter (YearMonth y my) = YearQuarter y $ monthOfYearQuarter my

dayQuarter :: Day -> Quarter
dayQuarter (MonthDay m _) = monthQuarter m
