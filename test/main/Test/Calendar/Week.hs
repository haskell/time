module Test.Calendar.Week (
    testWeek,
) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

testDay :: TestTree
testDay =
    nameTest "day" $ do
        let day = fromGregorian 2018 1 9
        assertEqual "" (ModifiedJulianDay 58127) day
        assertEqual "" (2018, 2, 2) $ toWeekDate day
        assertEqual "" Tuesday $ dayOfWeek day

allDaysOfWeek :: [DayOfWeek]
allDaysOfWeek = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

testAllDays :: String -> (DayOfWeek -> IO ()) -> TestTree
testAllDays name f = nameTest name $ fmap (\wd -> nameTest (show wd) $ f wd) allDaysOfWeek

testSucc :: TestTree
testSucc = testAllDays "succ" $ \wd -> assertEqual "" (toEnum $ succ $ fromEnum wd) $ succ wd

testPred :: TestTree
testPred = testAllDays "pred" $ \wd -> assertEqual "" (toEnum $ pred $ fromEnum wd) $ pred wd

testSequences :: TestTree
testSequences =
    nameTest
        "sequence"
        [ nameTest "[Monday .. Sunday]" $ assertEqual "" allDaysOfWeek [Monday .. Sunday]
        , nameTest "[Wednesday .. Wednesday]" $ assertEqual "" [Wednesday] [Wednesday .. Wednesday]
        , nameTest "[Sunday .. Saturday]" $
            assertEqual "" [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday] [Sunday .. Saturday]
        , nameTest "[Thursday .. Wednesday]" $
            assertEqual "" [Thursday, Friday, Saturday, Sunday, Monday, Tuesday, Wednesday] [Thursday .. Wednesday]
        , nameTest "[Tuesday ..]" $
            assertEqual
                ""
                [ Tuesday
                , Wednesday
                , Thursday
                , Friday
                , Saturday
                , Sunday
                , Monday
                , Tuesday
                , Wednesday
                , Thursday
                , Friday
                , Saturday
                , Sunday
                , Monday
                , Tuesday
                ]
                $ take 15 [Tuesday ..]
        , nameTest "[Wednesday, Tuesday ..]" $
            assertEqual
                ""
                [ Wednesday
                , Tuesday
                , Monday
                , Sunday
                , Saturday
                , Friday
                , Thursday
                , Wednesday
                , Tuesday
                , Monday
                , Sunday
                , Saturday
                , Friday
                , Thursday
                , Wednesday
                ]
                $ take 15 [Wednesday, Tuesday ..]
        , nameTest "[Sunday, Friday ..]" $
            assertEqual "" [Sunday, Friday, Wednesday, Monday, Saturday, Thursday, Tuesday, Sunday] $
                take 8 [Sunday, Friday ..]
        , nameTest "[Monday,Sunday .. Tuesday]" $
            assertEqual "" [Monday, Sunday, Saturday, Friday, Thursday, Wednesday, Tuesday] [Monday, Sunday .. Tuesday]
        , nameTest "[Thursday, Saturday .. Tuesday]" $
            assertEqual "" [Thursday, Saturday, Monday, Wednesday, Friday, Sunday, Tuesday] [Thursday, Saturday .. Tuesday]
        ]

testReadShow :: TestTree
testReadShow = testAllDays "read show" $ \wd -> assertEqual "" wd $ read $ show wd

prop_firstDayOfWeekOnAfter_onAfter :: DayOfWeek -> Day -> Bool
prop_firstDayOfWeekOnAfter_onAfter dw d = firstDayOfWeekOnAfter dw d >= d

prop_firstDayOfWeekOnAfter_Day :: DayOfWeek -> Day -> Bool
prop_firstDayOfWeekOnAfter_Day dw d = dayOfWeek (firstDayOfWeekOnAfter dw d) == dw

prop_toFromWeekCalendar :: FirstWeekType -> DayOfWeek -> Day -> Bool
prop_toFromWeekCalendar wt ws d =
    let (y, wy, dw) = toWeekCalendar wt ws d
     in fromWeekCalendar wt ws y wy dw == d

prop_weekChanges :: FirstWeekType -> DayOfWeek -> Day -> Bool
prop_weekChanges wt ws d =
    let (_, wy0, _) = toWeekCalendar wt ws d
        (_, wy1, dw) = toWeekCalendar wt ws $ succ d
     in if dw == ws then wy0 /= wy1 else wy0 == wy1

prop_weekYearWholeStart :: DayOfWeek -> Year -> Bool
prop_weekYearWholeStart ws y =
    let d = fromWeekCalendar FirstWholeWeek ws y 1 ws
        (y', dy) = toOrdinalDate d
     in y == y' && dy >= 1 && dy <= 7

prop_weekYearMostStart :: DayOfWeek -> Year -> Bool
prop_weekYearMostStart ws y =
    let d = fromWeekCalendar FirstMostWeek ws y 2 ws
        (y', dy) = toOrdinalDate d
     in y == y' && dy >= 5 && dy <= 11

testDiff :: TestTree
testDiff =
    nameTest
        "diff"
        [ nameTest "Friday - Tuesday" $ assertEqual "" 3 $ dayOfWeekDiff Friday Tuesday
        , nameTest "Tuesday - Friday" $ assertEqual "" 4 $ dayOfWeekDiff Tuesday Friday
        , nameTest "firstDayOfWeekOnAfter_onAfter" prop_firstDayOfWeekOnAfter_onAfter
        , nameTest "firstDayOfWeekOnAfter_Day" prop_firstDayOfWeekOnAfter_Day
        , nameTest "toFromWeekCalendar" prop_toFromWeekCalendar
        , nameTest "weekChanges" prop_weekChanges
        , nameTest "weekYearWholeStart" prop_weekYearWholeStart
        , nameTest "weekYearMostStart" prop_weekYearMostStart
        ]

testWeekDays :: TestTree
testWeekDays =
    nameTest
        "Week Days"
        [ testGroup "weekAllDays" weekAllDaysTests
        , testGroup "weekFirstDay" weekFirstDayTests
        , testGroup "weekLastDay" weekLastDayTests
        ]

weekAllDaysTests :: [TestTree]
weekAllDaysTests =
    [ testGroup
        "Property Tests"
        [ nameTest "Week have 7 days" weekHaveSevenDays
        , nameTest "Day is part of the week" dayIsPartOfWeek
        ]
    , testGroup
        "Unit Tests"
        [ nameTest "FirstDay is less than Day-DayOfWeek" $
            assertEqual
                ""
                [YearMonthDay 2023 12 31 .. YearMonthDay 2024 1 6]
                (weekAllDays Sunday (YearMonthDay 2024 1 1))
        , nameTest "FirstDay is equal to Day-DayOfWeek" $
            assertEqual
                ""
                [YearMonthDay 2024 2 26 .. YearMonthDay 2024 3 3]
                (weekAllDays Monday (YearMonthDay 2024 2 26))
        , nameTest "FirstDay is greater than Day-DayOfWeek" $
            assertEqual
                ""
                [YearMonthDay 2022 2 15 .. YearMonthDay 2022 2 21]
                (weekAllDays Tuesday (YearMonthDay 2022 2 21))
        ]
    ]
  where
    weekHaveSevenDays :: DayOfWeek -> Day -> Bool
    weekHaveSevenDays fd d = length (weekAllDays fd d) == 7

    dayIsPartOfWeek :: DayOfWeek -> Day -> Bool
    dayIsPartOfWeek fd d = d `elem` weekAllDays fd d

weekFirstDayTests :: [TestTree]
weekFirstDayTests =
    [ testGroup
        "Property Tests"
        [ nameTest "FirsyDay matches the Day-DayOfWeek" firstDayMatchesDay
        ]
    , testGroup
        "Unit Tests"
        [ nameTest "FirstDay is less than Day-DayOfWeek" $
            assertEqual
                ""
                (YearMonthDay 2022 2 20)
                (weekFirstDay Sunday (YearMonthDay 2022 2 21))
        , nameTest "FirstDay is equal to Day-DayOfWeek" $
            assertEqual
                ""
                (YearMonthDay 2022 2 21)
                (weekFirstDay Monday (YearMonthDay 2022 2 21))
        , nameTest "FirstDay is greater than Day-DayOfWeek" $
            assertEqual
                ""
                (YearMonthDay 2022 2 15)
                (weekFirstDay Tuesday (YearMonthDay 2022 2 21))
        ]
    ]
  where
    firstDayMatchesDay :: DayOfWeek -> Day -> Bool
    firstDayMatchesDay fd d = dayOfWeek (weekFirstDay fd d) == fd

weekLastDayTests :: [TestTree]
weekLastDayTests =
    [ nameTest "FirstDay is less than Day-DayOfWeek" $
        assertEqual
            ""
            (YearMonthDay 2022 2 26)
            (weekLastDay Sunday (YearMonthDay 2022 2 21))
    , nameTest "FirstDay is equal to Day-DayOfWeek" $
        assertEqual
            ""
            (YearMonthDay 2022 2 27)
            (weekLastDay Monday (YearMonthDay 2022 2 21))
    , nameTest "FirstDay is greater than Day-DayOfWeek" $
        assertEqual
            ""
            (YearMonthDay 2022 2 21)
            (weekLastDay Tuesday (YearMonthDay 2022 2 21))
    ]

testWeek :: TestTree
testWeek =
    nameTest
        "Week"
        [ testDay
        , testSucc
        , testPred
        , testSequences
        , testReadShow
        , testDiff
        , testWeekDays
        ]
