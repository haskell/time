module Test.Tests where

import Distribution.TestSuite

import Test.AddDays
import Test.ClipDates
import Test.ConvertBack
import Test.LongWeekYears
import Test.TestCalendars
import Test.TestEaster
import Test.TestFormat
import Test.TestMonthDay
import Test.TestParseDAT
import Test.TestTime

tests :: IO [Test]
tests = return [ addDaysTest
        , clipDates
        , convertBack
        , longWeekYears
        , testCalendars
        , testEaster
        , testFormat
        , testMonthDay
        , testParseDAT
        , testTime ]
