module Test.Calendar.MonthDay (
    testMonthDay,
) where

import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Calendar.MonthDay
import Test.Calendar.MonthDayRef
import Test.Tasty
import Test.Tasty.HUnit

showCompare :: (Eq a, Show a) => a -> String -> a -> String
showCompare a1 b a2
    | a1 == a2 = (show a1) ++ " == " ++ b
showCompare a1 b a2 = "DIFF: " ++ (show a1) ++ " -> " ++ b ++ " -> " ++ (show a2)

testMonthDay :: TestTree
testMonthDay =
    testGroup
        "MonthDay"
        [ testCase "good" $
            assertEqual "" testMonthDayRef $
                concat $
                    map (\isL -> unlines (leap isL : yearDays isL)) [False, True]
        , testGroup
            "clip"
            [ testCase "12" $ assertEqual "" (YearMonthDay 2005 05 12) (MonthDay (MkMonth 24064) 12)
            , testCase "1" $ assertEqual "" (YearMonthDay 2005 05 1) (MonthDay (MkMonth 24064) 1)
            , testCase "0" $ assertEqual "" (YearMonthDay 2005 05 1) (MonthDay (MkMonth 24064) 0)
            , testCase "-1" $ assertEqual "" (YearMonthDay 2005 05 1) (MonthDay (MkMonth 24064) (-1))
            , testCase "31" $ assertEqual "" (YearMonthDay 2005 05 31) (MonthDay (MkMonth 24064) 31)
            , testCase "32" $ assertEqual "" (YearMonthDay 2005 05 31) (MonthDay (MkMonth 24064) 32)
            , testCase "33" $ assertEqual "" (YearMonthDay 2005 05 31) (MonthDay (MkMonth 24064) 33)
            ]
        ]
  where
    leap isLeap =
        if isLeap
            then "Leap:"
            else "Regular:"
    yearDays isLeap =
        map
            ( \yd ->
                let
                    (m, d) = dayOfYearToMonthAndDay isLeap yd
                    yd' = monthAndDayToDayOfYear isLeap m d
                    mdtext = show m ++ "-" ++ show d
                in
                    showCompare yd mdtext yd'
            )
            [-2 .. 369]
