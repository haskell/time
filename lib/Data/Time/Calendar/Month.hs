{-# LANGUAGE Safe #-}

-- | An absolute count of common calendar months.
module Data.Time.Calendar.Month (
    Month (..),
    addMonths,
    diffMonths,
    pattern YearMonth,
    fromYearMonthValid,
    pattern MonthDay,
    fromMonthDayValid,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Ix
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Private
import Text.ParserCombinators.ReadP
import Text.Read

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable)

instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

-- | Show as @yyyy-mm@.
instance Show Month where
    show (YearMonth y m) = show4 y ++ "-" ++ show2 m

-- | Read as @yyyy-mm@.
instance Read Month where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ YearMonth y m

instance HasDays Month where
    firstDayOf (YearMonth y m) = YearMonthDay y m 1
    lastDayOf (YearMonth y m) = YearMonthDay y m 31

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

-- | Bidirectional abstract constructor.
-- Invalid months of year will be clipped to the correct range.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <-
    MkMonth ((\m -> divMod' m 12) -> (y, succ . fromInteger -> my))
    where
        YearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

fromYearMonthValid :: Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y my = do
    my' <- clipValid 1 12 my
    return $ YearMonth y my'

{-# COMPLETE YearMonth #-}

toMonthDay :: Day -> (Month, DayOfMonth)
toMonthDay (YearMonthDay y my dm) = (YearMonth y my, dm)

-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <-
    (toMonthDay -> (m, dm))
    where
        MonthDay (YearMonth y my) dm = YearMonthDay y my dm

fromMonthDayValid :: Month -> DayOfMonth -> Maybe Day
fromMonthDayValid (YearMonth y my) dm = fromGregorianValid y my dm

{-# COMPLETE MonthDay #-}
