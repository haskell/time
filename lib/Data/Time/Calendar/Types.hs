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

-- | Day of month, in range 1 to 31.
type DayOfMonth = Int

-- | Day of quarter, in range 1 to 92.
type DayOfQuarter = Int

-- | Day of year, in range 1 (January 1st) to 366.
-- December 31st is 365 in a common year, 366 in a leap year.
type DayOfYear = Int

-- | Week of year, by various reckonings, generally in range 0-53 depending on reckoning.
type WeekOfYear = Int
