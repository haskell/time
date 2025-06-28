{-# OPTIONS -fno-warn-orphans #-}

module Test.Format.ISO8601 (
    testISO8601,
) where

import Data.Coerce
import Data.Ratio
import Data.Time
import Data.Time.Format.ISO8601
import Test.Arbitrary ()
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)
import Test.TestUtil

deriving instance Eq ZonedTime

readShowProperty :: (Eq a, Show a) => (a -> Bool) -> Format a -> a -> Property
readShowProperty skip _ val | skip val = property Discard
readShowProperty _ fmt val =
    case formatShowM fmt val of
        Nothing -> property Discard
        Just str ->
            let
                found = formatParseM fmt str
                expected = Just val
            in
                property $
                    if expected == found
                        then succeeded
                        else failed{reason = show str ++ ": expected " ++ (show expected) ++ ", found " ++ (show found)}

class SpecialTestValues a where
    testGen :: Gen a
    default testGen :: Arbitrary a => Gen a
    testGen = arbitrary

    -- | values that should always be tested
    specialTestValues :: [a]
    specialTestValues = []

instance {-# OVERLAPPABLE #-} Arbitrary a => SpecialTestValues a

instance SpecialTestValues TimeOfDay where
    specialTestValues = [TimeOfDay 0 0 0, TimeOfDay 0 0 60, TimeOfDay 1 0 60, TimeOfDay 24 0 0]

instance SpecialTestValues (Integer, Int) where
    testGen = do
        Small y <- arbitrary
        woy <- choose (-10, 120)
        pure (y, woy)

readShowTestCheck :: (Eq a, Show a, SpecialTestValues a) => (a -> Bool) -> Format a -> [TestTree]
readShowTestCheck skip fmt =
    [ nameTest "random" $ fmap (readShowProperty skip fmt) testGen
    , nameTest "special" $ fmap (\a -> nameTest (show a) $ readShowProperty skip fmt a) $ filter (not . skip) specialTestValues
    ]

readShowTest :: (Eq a, Show a, SpecialTestValues a) => Format a -> [TestTree]
readShowTest = readShowTestCheck $ \_ -> False

readBoth :: NameTest t => (FormatExtension -> t) -> [TestTree]
readBoth fmts = [nameTest "extended" $ fmts ExtendedFormat, nameTest "basic" $ fmts BasicFormat]

readShowTestsCheck :: (Eq a, Show a, SpecialTestValues a) => (a -> Bool) -> (FormatExtension -> Format a) -> [TestTree]
readShowTestsCheck skip fmts = readBoth $ \fe -> readShowTestCheck skip $ fmts fe

readShowTests :: (Eq a, Show a, SpecialTestValues a) => (FormatExtension -> Format a) -> [TestTree]
readShowTests = readShowTestsCheck $ \_ -> False

newtype Durational t = MkDurational t
    deriving Eq

instance Show t => Show (Durational t) where
    show (MkDurational t) = show t

instance Arbitrary (Durational CalendarDiffDays) where
    arbitrary = do
        mm <- choose (-10000, 10000)
        dd <- choose (-40, 40)
        return $ MkDurational $ CalendarDiffDays mm dd

instance Arbitrary (Durational CalendarDiffTime) where
    arbitrary =
        let
            limit = 40 * 86400
            picofactor = 10 ^ (12 :: Int)
        in
            do
                mm <- choose (-10000, 10000)
                ss <- choose (negate limit * picofactor, limit * picofactor)
                return $ MkDurational $ CalendarDiffTime mm $ fromRational $ ss % picofactor

durationalFormat :: Format a -> Format (Durational a)
durationalFormat = coerce

testReadShowFormat :: TestTree
testReadShowFormat =
    localOption (QuickCheckTests 1000) $
        nameTest
            "read-show format"
            [ nameTest "calendarFormat" $ readShowTests $ calendarFormat
            , nameTest "yearMonthFormat" $ readShowTest $ yearMonthFormat
            , nameTest "yearFormat" $ readShowTest $ yearFormat
            , nameTest "centuryFormat" $ readShowTest $ centuryFormat
            , nameTest "expandedCalendarFormat" $ readShowTests $ expandedCalendarFormat 6
            , nameTest "expandedYearMonthFormat" $ readShowTest $ expandedYearMonthFormat 6
            , nameTest "expandedYearFormat" $ readShowTest $ expandedYearFormat 6
            , nameTest "expandedCenturyFormat" $ readShowTest $ expandedCenturyFormat 4
            , nameTest "ordinalDateFormat" $ readShowTests $ ordinalDateFormat
            , nameTest "expandedOrdinalDateFormat" $ readShowTests $ expandedOrdinalDateFormat 6
            , nameTest "weekDateFormat" $ readShowTests $ weekDateFormat
            , nameTest "yearWeekFormat" $ readShowTests $ yearWeekFormat
            , nameTest "expandedWeekDateFormat" $ readShowTests $ expandedWeekDateFormat 6
            , nameTest "expandedYearWeekFormat" $ readShowTests $ expandedYearWeekFormat 6
            , nameTest "timeOfDayFormat" $ readShowTests $ timeOfDayFormat
            , nameTest "hourMinuteFormat" $ readShowTestsCheck (\(TimeOfDay _ _ s) -> s >= 60) $ hourMinuteFormat
            , nameTest "hourFormat" $ readShowTestCheck (\(TimeOfDay _ _ s) -> s >= 60) $ hourFormat
            , nameTest "withTimeDesignator" $ readShowTests $ \fe -> withTimeDesignator $ timeOfDayFormat fe
            , nameTest "withUTCDesignator" $ readShowTests $ \fe -> withUTCDesignator $ timeOfDayFormat fe
            , nameTest "timeOffsetFormat" $ readShowTests $ timeOffsetFormat
            , nameTest "timeOfDayAndOffsetFormat" $ readShowTests $ timeOfDayAndOffsetFormat
            , nameTest "localTimeFormat" $
                readShowTests $
                    \fe -> localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
            , nameTest "zonedTimeFormat" $
                readShowTests $
                    \fe -> zonedTimeFormat (calendarFormat fe) (timeOfDayFormat fe) fe
            , nameTest "utcTimeFormat" $ readShowTests $ \fe -> utcTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
            , nameTest "dayAndTimeFormat" $
                readShowTests $
                    \fe -> dayAndTimeFormat (calendarFormat fe) (timeOfDayFormat fe)
            , nameTest "timeAndOffsetFormat" $ readShowTests $ \fe -> timeAndOffsetFormat (timeOfDayFormat fe) fe
            , nameTest "durationDaysFormat" $ readShowTest $ durationDaysFormat
            , nameTest "durationTimeFormat" $ readShowTest $ durationTimeFormat
            , nameTest "alternativeDurationDaysFormat" $
                readBoth $
                    \fe -> readShowTest (durationalFormat $ alternativeDurationDaysFormat fe)
            , nameTest "alternativeDurationTimeFormat" $
                readBoth $
                    \fe -> readShowTest (durationalFormat $ alternativeDurationTimeFormat fe)
            , nameTest "intervalFormat" $
                readShowTests $ \fe ->
                    intervalFormat (localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)) durationTimeFormat
            , nameTest "recurringIntervalFormat" $
                readShowTests $ \fe ->
                    recurringIntervalFormat (localTimeFormat (calendarFormat fe) (timeOfDayFormat fe)) durationTimeFormat
            ]

testShowReadFormat :: (Show t, Eq t) => String -> Format t -> String -> t -> TestTree
testShowReadFormat name fmt str val =
    nameTest
        (name ++ ": " ++ str)
        [ nameTest "show" $ assertEqual "" (Just str) $ formatShowM fmt val
        , nameTest "read" $ assertEqual "" (Just val) $ formatParseM fmt str
        ]

testReadFormat :: (Show t, Eq t) => String -> Format t -> String -> t -> TestTree
testReadFormat name fmt str val = nameTest (name ++ ": " ++ str) $ assertEqual "" (Just val) $ formatParseM fmt str

testShowFormats :: TestTree
testShowFormats =
    nameTest
        "show format"
        [ testShowReadFormat "durationDaysFormat" durationDaysFormat "P0D" $ CalendarDiffDays 0 0
        , testShowReadFormat "durationDaysFormat" durationDaysFormat "P4Y" $ CalendarDiffDays 48 0
        , testShowReadFormat "durationDaysFormat" durationDaysFormat "P7M" $ CalendarDiffDays 7 0
        , testShowReadFormat "durationDaysFormat" durationDaysFormat "P5D" $ CalendarDiffDays 0 5
        , testShowReadFormat "durationDaysFormat" durationDaysFormat "P2Y3M81D" $ CalendarDiffDays 27 81
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P0D" $ CalendarDiffTime 0 0
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P4Y" $ CalendarDiffTime 48 0
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P7M" $ CalendarDiffTime 7 0
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P5D" $ CalendarDiffTime 0 $ 5 * nominalDay
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P2Y3M81D" $ CalendarDiffTime 27 $ 81 * nominalDay
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "PT2H" $ CalendarDiffTime 0 $ 7200
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "PT3M" $ CalendarDiffTime 0 $ 180
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "PT12S" $ CalendarDiffTime 0 $ 12
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "PT1M18.77634S" $ CalendarDiffTime 0 $ 78.77634
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "PT2H1M18.77634S" $ CalendarDiffTime 0 $ 7278.77634
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P5DT2H1M18.77634S" $
            CalendarDiffTime 0 $
                5 * nominalDay + 7278.77634
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P7Y10M5DT2H1M18.77634S" $
            CalendarDiffTime 94 $
                5 * nominalDay + 7278.77634
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P7Y10MT2H1M18.77634S" $
            CalendarDiffTime 94 $
                7278.77634
        , testShowReadFormat "durationTimeFormat" durationTimeFormat "P8YT2H1M18.77634S" $ CalendarDiffTime 96 $ 7278.77634
        , testShowReadFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0001-00-00" $
            CalendarDiffDays 12 0
        , testShowReadFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0002-03-29" $
            CalendarDiffDays 27 29
        , testShowReadFormat "alternativeDurationDaysFormat" (alternativeDurationDaysFormat ExtendedFormat) "P0561-08-29" $
            CalendarDiffDays (561 * 12 + 8) 29
        , testShowReadFormat
            "alternativeDurationTimeFormat"
            (alternativeDurationTimeFormat ExtendedFormat)
            "P0000-00-01T00:00:00"
            $ CalendarDiffTime 0 86400
        , testShowReadFormat
            "alternativeDurationTimeFormat"
            (alternativeDurationTimeFormat ExtendedFormat)
            "P0007-10-05T02:01:18.77634"
            $ CalendarDiffTime 94
            $ 5 * nominalDay + 7278.77634
        , testShowReadFormat
            "alternativeDurationTimeFormat"
            (alternativeDurationTimeFormat ExtendedFormat)
            "P4271-10-05T02:01:18.77634"
            $ CalendarDiffTime (12 * 4271 + 10)
            $ 5 * nominalDay + 7278.77634
        , testShowReadFormat "centuryFormat" centuryFormat "02" 2
        , testShowReadFormat "centuryFormat" centuryFormat "21" 21
        , testShowReadFormat
            "intervalFormat etc."
            ( intervalFormat
                (localTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat))
                durationTimeFormat
            )
            "2015-06-13T21:13:56/P1Y2M7DT5H33M2.34S"
            ( LocalTime (fromGregorian 2015 6 13) (TimeOfDay 21 13 56)
            , CalendarDiffTime 14 $ 7 * nominalDay + 5 * 3600 + 33 * 60 + 2.34
            )
        , testShowReadFormat
            "recurringIntervalFormat etc."
            ( recurringIntervalFormat
                (localTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat))
                durationTimeFormat
            )
            "R74/2015-06-13T21:13:56/P1Y2M7DT5H33M2.34S"
            ( 74
            , LocalTime (fromGregorian 2015 6 13) (TimeOfDay 21 13 56)
            , CalendarDiffTime 14 $ 7 * nominalDay + 5 * 3600 + 33 * 60 + 2.34
            )
        , testShowReadFormat
            "recurringIntervalFormat etc."
            (recurringIntervalFormat (calendarFormat ExtendedFormat) durationDaysFormat)
            "R74/2015-06-13/P1Y2M7D"
            (74, fromGregorian 2015 6 13, CalendarDiffDays 14 7)
        , testShowReadFormat "timeOffsetFormat" iso8601Format "-06:30" (minutesToTimeZone (-390))
        , testShowReadFormat "timeOffsetFormat" iso8601Format "-06:00" (minutesToTimeZone (-360))
        , testReadFormat "timeOffsetFormat" iso8601Format "-06" (minutesToTimeZone (-360))
        , testShowReadFormat "timeOffsetFormat" iso8601Format "+11:00" (minutesToTimeZone 660)
        , testReadFormat "timeOffsetFormat" iso8601Format "+11" (minutesToTimeZone 660)
        , testShowReadFormat "timeOffsetFormat" iso8601Format "+00:00" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" iso8601Format "+00" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" iso8601Format "-00:00" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" iso8601Format "-00" (minutesToTimeZone 0)
        , testShowReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+0000" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+00" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "-0000" (minutesToTimeZone 0)
        , testReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "-00" (minutesToTimeZone 0)
        , testShowReadFormat "timeOffsetFormat" iso8601Format "+00:10" (minutesToTimeZone 10)
        , testShowReadFormat "timeOffsetFormat" iso8601Format "-00:10" (minutesToTimeZone (-10))
        , testShowReadFormat "timeOffsetFormat" iso8601Format "+01:35" (minutesToTimeZone 95)
        , testShowReadFormat "timeOffsetFormat" iso8601Format "-01:35" (minutesToTimeZone (-95))
        , testShowReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+0135" (minutesToTimeZone 95)
        , testShowReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "-0135" (minutesToTimeZone (-95))
        , testShowReadFormat
            "timeOffsetFormat"
            (timeOffsetFormat BasicFormat)
            "-1100"
            (minutesToTimeZone $ negate $ 11 * 60)
        , testShowReadFormat "timeOffsetFormat" (timeOffsetFormat BasicFormat) "+1015" (minutesToTimeZone $ 615)
        , testShowReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553-06:30"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone (-390)))
        , testShowReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553-06:00"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone (-360)))
        , testReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553-06"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone (-360)))
        , testShowReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553+06:30"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone 390))
        , testShowReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553+06:00"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone 360))
        , testReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553+06"
            (ZonedTime (LocalTime (fromGregorian 2024 07 06) (TimeOfDay 8 45 56.553)) (minutesToTimeZone 360))
        , testReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-05-30T16:15:18Z"
            (ZonedTime (LocalTime (fromGregorian 2024 05 30) (TimeOfDay 16 15 18)) utc)
        , testReadFormat
            "zonedTimeFormat"
            iso8601Format
            "2024-05-30T16:15:18"
            (ZonedTime (LocalTime (fromGregorian 2024 05 30) (TimeOfDay 16 15 18)) utc)
        , testShowReadFormat
            "utcTimeFormat"
            iso8601Format
            "2024-07-06T08:45:56.553Z"
            (UTCTime (fromGregorian 2024 07 06) (timeOfDayToTime $ TimeOfDay 8 45 56.553))
        , testShowReadFormat
            "utcTimeFormat"
            iso8601Format
            "2028-12-31T23:59:60.9Z"
            (UTCTime (fromGregorian 2028 12 31) (timeOfDayToTime $ TimeOfDay 23 59 60.9))
        , testReadFormat
            "utcTimeFormat"
            iso8601Format
            "2028-12-31T23:59:60.9"
            (UTCTime (fromGregorian 2028 12 31) (timeOfDayToTime $ TimeOfDay 23 59 60.9))
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1994-W52-7" (fromGregorian 1995 1 1)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1995-W01-1" (fromGregorian 1995 1 2)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1996-W52-7" (fromGregorian 1996 12 29)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1997-W01-2" (fromGregorian 1996 12 31)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1997-W01-3" (fromGregorian 1997 1 1)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1974-W32-6" (fromGregorian 1974 8 10)
        , testShowReadFormat "weekDateFormat" (weekDateFormat BasicFormat) "1974W326" (fromGregorian 1974 8 10)
        , testShowReadFormat "weekDateFormat" (weekDateFormat ExtendedFormat) "1995-W05-6" (fromGregorian 1995 2 4)
        , testShowReadFormat "weekDateFormat" (weekDateFormat BasicFormat) "1995W056" (fromGregorian 1995 2 4)
        , testShowReadFormat
            "weekDateFormat"
            (expandedWeekDateFormat 6 ExtendedFormat)
            "+001995-W05-6"
            (fromGregorian 1995 2 4)
        , testShowReadFormat "weekDateFormat" (expandedWeekDateFormat 6 BasicFormat) "+001995W056" (fromGregorian 1995 2 4)
        , testShowReadFormat "ordinalDateFormat" (ordinalDateFormat ExtendedFormat) "1846-235" (fromGregorian 1846 8 23)
        , testShowReadFormat "ordinalDateFormat" (ordinalDateFormat BasicFormat) "1844236" (fromGregorian 1844 8 23)
        , testShowReadFormat
            "ordinalDateFormat"
            (expandedOrdinalDateFormat 5 ExtendedFormat)
            "+01846-235"
            (fromGregorian 1846 8 23)
        , testShowReadFormat "hourMinuteFormat" (hourMinuteFormat ExtendedFormat) "13:17.25" (TimeOfDay 13 17 15)
        , testShowReadFormat "hourMinuteFormat" (hourMinuteFormat ExtendedFormat) "01:12.4" (TimeOfDay 1 12 24)
        , testShowReadFormat "hourMinuteFormat" (hourMinuteFormat BasicFormat) "1317.25" (TimeOfDay 13 17 15)
        , testShowReadFormat "hourMinuteFormat" (hourMinuteFormat BasicFormat) "0112.4" (TimeOfDay 1 12 24)
        , testShowReadFormat "hourFormat" hourFormat "22" (TimeOfDay 22 0 0)
        , testShowReadFormat "hourFormat" hourFormat "06" (TimeOfDay 6 0 0)
        , testShowReadFormat "hourFormat" hourFormat "18.9475" (TimeOfDay 18 56 51)
        ]

testISO8601 :: TestTree
testISO8601 = nameTest "ISO8601" [testShowFormats, testReadShowFormat]
