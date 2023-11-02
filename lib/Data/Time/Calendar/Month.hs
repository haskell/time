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
import Data.Maybe (fromJust)
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Private
import Data.Time.Calendar.Types
import qualified Language.Haskell.TH.Syntax as TH
import Text.ParserCombinators.ReadP
import Text.Read

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable, TH.Lift)

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
    show (YearMonth y m) = show4 y ++ "-" ++ show2 (monthOfYearIndex m)

-- | Read as @yyyy-mm@.
instance Read Month where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        moy' <- readPrec
        moy <- case parseMonthOfYearIndex moy' of
          Nothing -> fail "Invalid month of year index"
          Just moy -> pure moy
        return $ YearMonth y moy

instance DayPeriod Month where
    periodFirstDay (YearMonth y m) = YearMonthDay y m 1
    periodLastDay (YearMonth y m) = YearMonthDay y m 31 -- clips to correct day
    dayPeriod (YearMonthDay y my _) = YearMonth y my

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

-- | Bidirectional abstract constructor.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <-
    MkMonth ((\m -> divMod' m 12) -> (y, fromJust . parseMonthOfYearIndex . succ . fromInteger -> my))
    where
        YearMonth y my = MkMonth $ (y * 12) + toInteger (monthOfYearIndex (pred my))

fromYearMonthValid :: Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y moy = Just $ fromYearMonth y moy

fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth = YearMonth

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
