module Test.Calendar.MonthOfYear (
    testMonthOfYear,
) where

import Data.Foldable
import Data.Time.Calendar
import Test.Tasty
import Test.Tasty.HUnit

matchMonthOfYear :: MonthOfYear -> Int
matchMonthOfYear m = case m of
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12

testMonthOfYear :: TestTree
testMonthOfYear = testCase "MonthOfYear" $ for_ [1 .. 12] $ \m -> assertEqual (show m) m $ matchMonthOfYear m
