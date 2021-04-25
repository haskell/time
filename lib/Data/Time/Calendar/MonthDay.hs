{-# LANGUAGE Safe #-}

module Data.Time.Calendar.MonthDay (
    MonthOfYear,
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
    month' = clip 1 12 month
    day' = fromIntegral (clip 1 (monthLength' isLeap month') day)
    month'' = fromIntegral month'
    k =
        if month' <= 2
            then 0
            else
                if isLeap
                    then -1
                    else -2

-- | Convert month and day in the Gregorian or Julian calendars to day of year.
-- First arg is leap year flag.
monthAndDayToDayOfYearValid :: Bool -> MonthOfYear -> DayOfMonth -> Maybe DayOfYear
monthAndDayToDayOfYearValid isLeap month day = do
    month' <- clipValid 1 12 month
    day' <- clipValid 1 (monthLength' isLeap month') day
    let day'' = fromIntegral day'
        month'' = fromIntegral month'
        k =
            if month' <= 2
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

findMonthDay :: [Int] -> Int -> (Int, Int)
findMonthDay (n : ns) yd
    | yd > n = (\(m, d) -> (m + 1, d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1, yd)

-- | The length of a given month in the Gregorian or Julian calendars.
-- First arg is leap year flag.
monthLength :: Bool -> MonthOfYear -> DayOfMonth
monthLength isLeap month' = monthLength' isLeap (clip 1 12 month')

monthLength' :: Bool -> MonthOfYear -> DayOfMonth
monthLength' isLeap month' = (monthLengths isLeap) !! (month' - 1)

monthLengths :: Bool -> [DayOfMonth]
monthLengths isleap =
    [ 31
    , if isleap
        then 29
        else 28
    , 31
    , 30
    , 31
    , 30
    , 31
    , 31
    , 30
    , 31
    , 30
    , 31
    ]

--J        F                   M  A  M  J  J  A  S  O  N  D
