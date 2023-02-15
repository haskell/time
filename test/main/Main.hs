module Main where

import Test.Calendar.AddDays
import Test.Calendar.CalendarProps
import Test.Calendar.Calendars
import Test.Calendar.ClipDates
import Test.Calendar.ConvertBack
import Test.Calendar.DayPeriod
import Test.Calendar.Duration
import Test.Calendar.Easter
import Test.Calendar.LongWeekYears
import Test.Calendar.MonthDay
import Test.Calendar.MonthOfYear
import Test.Calendar.Valid
import Test.Calendar.Week
import Test.Calendar.Year
import Test.Clock.Conversion
import Test.Clock.Lift (testLift)
import Test.Clock.Resolution
import Test.Clock.TAI
import Test.Format.Compile ()
import Test.Format.Format
import Test.Format.ISO8601
import Test.Format.ParseTime
import Test.LocalTime.CalendarDiffTime
import Test.LocalTime.Time
import Test.LocalTime.TimeOfDay
import Test.Tasty
import Test.Types ()

tests :: TestTree
tests =
    testGroup
        "Time"
        [ testGroup
            "Calendar"
            [ addDaysTest
            , testCalendarProps
            , testCalendars
            , clipDates
            , convertBack
            , longWeekYears
            , testDayPeriod
            , testMonthDay
            , testMonthOfYear
            , testEaster
            , testValid
            , testWeek
            , testYear
            , testDuration
            ]
        , testGroup "Clock" [testClockConversion, testResolutions, testTAI, testLift]
        , testGroup "Format" [testFormat, testParseTime, testISO8601]
        , testGroup "LocalTime" [testTime, testTimeOfDay, testCalendarDiffTime]
        ]

main :: IO ()
main = defaultMain tests
