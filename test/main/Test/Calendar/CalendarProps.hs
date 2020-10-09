#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif
module Test.Calendar.CalendarProps
    ( testCalendarProps
    ) where

import Data.Time.Calendar.MonthCount
import Test.TestUtil
import Test.Tasty
import Test.Arbitrary ()

testYearMonth :: TestTree
testYearMonth = nameTest "YearMonth" $ \m -> case m of
    YearMonth y my -> m == YearMonth y my

testMonthDay :: TestTree
testMonthDay = nameTest "MonthDay" $ \d -> case d of
    MonthDay m dm -> d == MonthDay m dm

testCalendarProps :: TestTree
testCalendarProps = nameTest "calender-props" [testYearMonth,testMonthDay]
