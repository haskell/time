{-# LANGUAGE Safe #-}

-- | Year quarters.
module Data.Time.Calendar.Quarter (
    QuarterOfYear (..),
    addQuarters,
    diffQuarters,
    Quarter (..),
    pattern YearQuarter,
    monthOfYearQuarter,
    monthQuarter,
    dayQuarter,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Ix
import Data.Time.Calendar.Days
import Data.Time.Calendar.Month
import Data.Time.Calendar.Private
import Data.Time.Calendar.Types
import Text.ParserCombinators.ReadP
import Text.Read

-- | Quarters of each year. Each quarter corresponds to three months.
data QuarterOfYear = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Data, Typeable, Read, Show, Ix)

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

instance NFData QuarterOfYear where
    rnf Q1 = ()
    rnf Q2 = ()
    rnf Q3 = ()
    rnf Q4 = ()

-- | An absolute count of year quarters.
-- Number is equal to @(year * 4) + (quarterOfYear - 1)@.
newtype Quarter = MkQuarter Integer deriving (Eq, Ord, Data, Typeable)

instance NFData Quarter where
    rnf (MkQuarter m) = rnf m

instance Enum Quarter where
    succ (MkQuarter a) = MkQuarter (succ a)
    pred (MkQuarter a) = MkQuarter (pred a)
    toEnum = MkQuarter . toEnum
    fromEnum (MkQuarter a) = fromEnum a
    enumFrom (MkQuarter a) = fmap MkQuarter (enumFrom a)
    enumFromThen (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromThen a b)
    enumFromTo (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromTo a b)
    enumFromThenTo (MkQuarter a) (MkQuarter b) (MkQuarter c) =
        fmap MkQuarter (enumFromThenTo a b c)

instance Ix Quarter where
    range (MkQuarter a, MkQuarter b) = fmap MkQuarter (range (a, b))
    index (MkQuarter a, MkQuarter b) (MkQuarter c) = index (a, b) c
    inRange (MkQuarter a, MkQuarter b) (MkQuarter c) = inRange (a, b) c
    rangeSize (MkQuarter a, MkQuarter b) = rangeSize (a, b)

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

instance HasDays Quarter where
    firstDayOf (YearQuarter y q) =
        case q of
            Q1 -> firstDayOf $ YearMonth y January
            Q2 -> firstDayOf $ YearMonth y April
            Q3 -> firstDayOf $ YearMonth y July
            Q4 -> firstDayOf $ YearMonth y October
    lastDayOf (YearQuarter y q) =
        case q of
            Q1 -> lastDayOf $ YearMonth y March
            Q2 -> lastDayOf $ YearMonth y June
            Q3 -> lastDayOf $ YearMonth y September
            Q4 -> lastDayOf $ YearMonth y December

addQuarters :: Integer -> Quarter -> Quarter
addQuarters n (MkQuarter a) = MkQuarter $ a + n

diffQuarters :: Quarter -> Quarter -> Integer
diffQuarters (MkQuarter a) (MkQuarter b) = a - b

-- | Bidirectional abstract constructor.
pattern YearQuarter :: Year -> QuarterOfYear -> Quarter
pattern YearQuarter y qy <-
    MkQuarter ((\q -> divMod' q 4) -> (y, toEnum . succ . fromInteger -> qy))
    where
        YearQuarter y qy = MkQuarter $ (y * 4) + toInteger (pred $ fromEnum qy)

{-# COMPLETE YearQuarter #-}

monthOfYearQuarter :: MonthOfYear -> QuarterOfYear
monthOfYearQuarter my | my <= 3 = Q1
monthOfYearQuarter my | my <= 6 = Q2
monthOfYearQuarter my | my <= 9 = Q3
monthOfYearQuarter _ = Q4

monthQuarter :: Month -> Quarter
monthQuarter (YearMonth y my) = YearQuarter y $ monthOfYearQuarter my

dayQuarter :: Day -> Quarter
dayQuarter (MonthDay m _) = monthQuarter m
