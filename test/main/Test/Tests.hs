module Test.Tests where

import Test.Tasty

import Test.Calendar.AddDays
import Test.Calendar.ClipDates
import Test.Clock.Conversion
import Test.Calendar.ConvertBack
import Test.Calendar.LongWeekYears
import Test.Clock.Resolution
import Test.Calendar.Calendars
import Test.Calendar.Easter
import Test.Format.Format
import Test.Calendar.MonthDay
import Test.Format.ParseTime
import Test.Clock.TAI
import Test.LocalTime.Time
import Test.LocalTime.TimeZone
import Test.Calendar.Valid

tests :: TestTree
tests = testGroup "time" [ addDaysTest
        , clipDates
        , testClockConversion
        , convertBack
        , longWeekYears
        , testResolution
        , testCalendars
        , testEaster
        , testFormat
        , testMonthDay
        , testParseTime
        , testTAI
        , testTime
        , testTimeZone
        , testValid ]
