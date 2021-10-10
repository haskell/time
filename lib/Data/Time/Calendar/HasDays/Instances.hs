{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Calendar.HasDays.Instances () where

import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.HasDays.Class
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter

instance HasDays Day where
    firstDay = id
    lastDay = id

instance HasDays Month where
    firstDay (YearMonth y m) = YearMonthDay y m 1
    lastDay (YearMonth y m) = YearMonthDay y m 31

instance HasDays Quarter where
    firstDay (YearQuarter y q) =
        case q of
            Q1 -> YearMonthDay y January 1
            Q2 -> YearMonthDay y April 1
            Q3 -> YearMonthDay y July 1
            Q4 -> YearMonthDay y October 1
    lastDay (YearQuarter y q) =
        case q of
            Q1 -> YearMonthDay y March 31
            Q2 -> YearMonthDay y June 30
            Q3 -> YearMonthDay y September 30
            Q4 -> YearMonthDay y December 31

instance HasDays Year where
    firstDay y = YearMonthDay y January 1
    lastDay y = YearMonthDay y December 31
