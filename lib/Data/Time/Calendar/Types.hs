{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Types where

import GHC.Generics (Generic)

-- | Year of Common Era (when positive).
type Year = Integer

-- | Also known as Anno Domini.
pattern CommonEra :: Integer -> Year
pattern CommonEra n <-
    ((\y -> if y > 0 then Just y else Nothing) -> Just n)
    where
        CommonEra n = n

-- | Also known as Before Christ.
-- Note that Year 1 = 1 CE, and the previous Year 0 = 1 BCE.
-- 'CommonEra' and 'BeforeCommonEra' form a @COMPLETE@ set.
pattern BeforeCommonEra :: Integer -> Year
pattern BeforeCommonEra n <-
    ((\y -> if y <= 0 then Just (1 - y) else Nothing) -> Just n)
    where
        BeforeCommonEra n = 1 - n

{-# COMPLETE CommonEra, BeforeCommonEra #-}

data MonthOfYear
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

-- | 1-based index of the month
monthOfYearIndex :: MonthOfYear -> Int
monthOfYearIndex moy = case moy of
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

parseMonthOfYearIndex :: Int -> Maybe MonthOfYear
parseMonthOfYearIndex ix = case ix of
  1 -> Just January
  2 -> Just February
  3 -> Just March
  4 -> Just April
  5 -> Just May
  6 -> Just June
  7 -> Just July
  8 -> Just August
  9 -> Just September
  10 -> Just October
  11 -> Just November
  12 -> Just December
  _ -> Nothing

-- | Day of month, in range 1 to 31.
type DayOfMonth = Int

-- | Day of quarter, in range 1 to 92.
type DayOfQuarter = Int

-- | Day of year, in range 1 (January 1st) to 366.
-- December 31st is 365 in a common year, 366 in a leap year.
type DayOfYear = Int

-- | Week of year, by various reckonings, generally in range 0-53 depending on reckoning.
type WeekOfYear = Int
