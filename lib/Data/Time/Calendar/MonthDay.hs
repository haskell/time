{-# LANGUAGE Safe #-}

module Data.Time.Calendar.MonthDay (
    MonthOfYear(..),
    DayOfMonth,
    DayOfYear,
    monthAndDayToDayOfYear,
    monthAndDayToDayOfYearValid,
    dayOfYearToMonthAndDay,
    monthLength,
) where

import Data.Time.Calendar.Private
import Data.Time.Calendar.Types

-- | Convert month and day in the Gregorian or Julian calendars to day of year.
-- First arg is leap year flag.
monthAndDayToDayOfYear :: Bool -> MonthOfYear -> DayOfMonth -> DayOfYear
monthAndDayToDayOfYear isLeap month day = (div (367 * month'' - 362) 12) + k + day'
  where
    day' = fromIntegral (clip 1 (monthLength isLeap month) day)
    month'' = monthOfYearIndex month
    k =
        if month <= February
            then 0
            else
                if isLeap
                    then -1
                    else -2

-- | Convert month and day in the Gregorian or Julian calendars to day of year.
-- First arg is leap year flag.
monthAndDayToDayOfYearValid :: Bool -> MonthOfYear -> DayOfMonth -> Maybe DayOfYear
monthAndDayToDayOfYearValid isLeap month day = do
    day' <- clipValid 1 (monthLength isLeap month) day
    let
        day'' = fromIntegral day'
        month'' = monthOfYearIndex month
        k =
            if month <= February
                then 0
                else
                    if isLeap
                        then -1
                        else -2
    return ((div (367 * month'' - 362) 12) + k + day'')

-- | Convert day of year in the Gregorian or Julian calendars to month and day.
-- First arg is leap year flag.
dayOfYearToMonthAndDay :: Bool -> DayOfYear -> (MonthOfYear, DayOfMonth)
dayOfYearToMonthAndDay isLeap yd =
    findMonthDay
        (monthLengths isLeap)
        ( clip
            1
            ( if isLeap
                then 366
                else 365
            )
            yd
        )

findMonthDay :: [Int] -> Int -> (MonthOfYear, Int)
findMonthDay (n : ns) yd
    | yd > n = (\(m, d) -> (succ m, d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (January, yd)

-- | The length of a given month in the Gregorian or Julian calendars.
-- First arg is leap year flag.
monthLength :: Bool -> MonthOfYear -> DayOfMonth
monthLength isLeap month = case month of
  January -> 31
  February -> if isLeap then 29 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

monthLengths :: Bool -> [Int]
monthLengths isLeap = map (monthLength isLeap) [minBound .. maxBound]
