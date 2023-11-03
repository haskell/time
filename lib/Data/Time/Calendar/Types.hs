{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Types where

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

-- | Month of year, in range 1 (January) to 12 (December).
type MonthOfYear = Int

pattern January :: MonthOfYear
pattern January = 1

pattern February :: MonthOfYear
pattern February = 2

pattern March :: MonthOfYear
pattern March = 3

pattern April :: MonthOfYear
pattern April = 4

pattern May :: MonthOfYear
pattern May = 5

pattern June :: MonthOfYear
pattern June = 6

pattern July :: MonthOfYear
pattern July = 7

pattern August :: MonthOfYear
pattern August = 8

pattern September :: MonthOfYear
pattern September = 9

pattern October :: MonthOfYear
pattern October = 10

pattern November :: MonthOfYear
pattern November = 11

-- | The twelve 'MonthOfYear' patterns form a @COMPLETE@ set.
pattern December :: MonthOfYear
pattern December = 12

-- | Day of month, in range 1 to 31.
type DayOfMonth = Int

-- | Day of quarter, in range 1 to 92.
type DayOfQuarter = Int

-- | Day of year, in range 1 (January 1st) to 366.
-- December 31st is 365 in a common year, 366 in a leap year.
type DayOfYear = Int

-- | Week of year, by various reckonings, generally in range 0-53 depending on reckoning.
type WeekOfYear = Int
