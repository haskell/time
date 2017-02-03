module Main where

import Foreign.C.Types
import Test.Tasty
import Test.Calendar.AddDays
import Test.Calendar.Calendars
import Test.Calendar.ClipDates
import Test.Calendar.ConvertBack
import Test.Calendar.Easter
import Test.Calendar.LongWeekYears
import Test.Calendar.MonthDay
import Test.Calendar.Valid
import Test.Clock.Conversion
import Test.Clock.Resolution
import Test.Clock.TAI
import Test.Format.Format
import Test.Format.ParseTime
import Test.LocalTime.Time
import Test.LocalTime.TimeZone


tests :: TestTree
tests = testGroup "Time" [
    testGroup "Calendar" [
        addDaysTest,
        testCalendars,
        clipDates,
        convertBack,
        longWeekYears,
        testMonthDay,
        testEaster,
        testValid
        ],
    testGroup "Clock" [
        testClockConversion,
        testResolution,
        testTAI
        ],
    testGroup "Format" [
        testFormat,
        testParseTime
        ],
    testGroup "LocalTime" [
        testTime,
        testTimeZone
        ]
    ]

main :: IO ()
main = do
  if (toRational (1000000000000 :: CTime)) /= (1000000000000 :: Rational)
    then putStrLn "WARNING: Some tests will incorrectly fail due to a 32-bit time_t C type."
    else return ()
  defaultMain tests
