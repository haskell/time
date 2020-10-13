{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}
#endif

-- | An absolute count of common calendar months.
module Data.Time.Calendar.Month
    (
        Month(..), addMonths, diffMonths,
        pattern YearMonth,
        fromYearMonthValid,
        pattern MonthDay,
        fromMonthDayValid
    ) where

import Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Private
import Data.Data
import Data.Fixed
import Text.Read
import Text.ParserCombinators.ReadP

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable)

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

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

-- | Bidirectional abstract constructor.
-- Invalid months of year will be clipped to the correct range.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <- MkMonth ((\m -> divMod' m 12) -> (y,succ . fromInteger -> my)) where
    YearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

fromYearMonthValid :: Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y my = do
    my' <- clipValid 1 12 my
    return $ YearMonth y my'

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearMonth #-}
#endif

toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay (YearMonthDay y my dm) = (YearMonth y my, dm)

-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <- (toMonthDay -> (m,dm)) where
    MonthDay (YearMonth y my) dm = YearMonthDay y my dm

fromMonthDayValid :: Month -> DayOfMonth -> Maybe Day
fromMonthDayValid (YearMonth y my) dm = fromGregorianValid y my dm

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE MonthDay #-}
#endif
