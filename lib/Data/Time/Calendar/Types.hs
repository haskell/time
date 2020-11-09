{-# LANGUAGE Safe #-}

module Data.Time.Calendar.Types where


-- | Year of Common Era.
type Year = Integer

-- | Month of year, in range 1 (January) to 12 (December).
type MonthOfYear = Int

-- | Day of month, in range 1 to 31.
type DayOfMonth = Int

-- | Day of year, in range 1 (January 1st) to 366.
-- December 31st is 365 in a common year, 366 in a leap year.
type DayOfYear = Int

-- | Week of year, by various reckonings, generally in range 0-53 depending on reckoning
type WeekOfYear = Int
