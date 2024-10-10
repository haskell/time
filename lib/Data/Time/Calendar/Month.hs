{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | An absolute count of common calendar months.
module Data.Time.Calendar.Month (
    Month (..),
    addMonths,
    diffMonths,
#if __GLASGOW_HASKELL__
    pattern YearMonth,
    fromYearMonthValid,
    pattern MonthDay,
    fromMonthDayValid,
#endif
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Ix
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Private
#if __GLASGOW_HASKELL__
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH
#endif
import Text.ParserCombinators.ReadP
import Text.Read

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable
#if __GLASGOW_HASKELL__
                                                                 , TH.Lift, Generic
#endif
                                                                                   )

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

#ifdef __GLASGOW_HASKELL__
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

instance DayPeriod Month where
    periodFirstDay (YearMonth y m) = YearMonthDay y m 1
    periodLastDay (YearMonth y m) = YearMonthDay y m 31 -- clips to correct day
    dayPeriod (YearMonthDay y my _) = YearMonth y my
#endif

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

#ifdef __GLASGOW_HASKELL__
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

-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <-
    (periodFromDay -> (m, dm))
    where
        MonthDay = periodToDay

fromMonthDayValid :: Month -> DayOfMonth -> Maybe Day
fromMonthDayValid = periodToDayValid

{-# COMPLETE MonthDay #-}
#endif
