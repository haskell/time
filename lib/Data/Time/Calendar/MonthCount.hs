{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif
module Data.Time.Calendar.MonthCount
    (
        IntegerAdditive(..),
        Month(..),
        pattern YearMonth,
        pattern MonthDay
    ) where

import Data.Time.Calendar
import Data.Time.Calendar.Private
import Data.IntegerAdditive
import Data.Data
import Data.Fixed
import Text.Read
import Text.ParserCombinators.ReadP

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, IntegerAdditive, Data, Typeable)

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

-- | Abstract constructor.
-- Invalid months of year will be clipped to the correct range.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <- MkMonth ((\m -> divMod' m 12) -> (y,succ . fromInteger -> my)) where
    YearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearMonth #-}
#endif

toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay (YearMonthDay y my dm) = (YearMonth y my, dm)

-- | Abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <- (toMonthDay -> (m,dm)) where
    MonthDay (YearMonth y my) dm = YearMonthDay y my dm

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE MonthDay #-}
#endif
