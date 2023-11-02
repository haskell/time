module Test.Calendar.AddDays (
    addDaysTest,
) where

import Data.Time.Calendar
import Test.Calendar.AddDaysRef
import Test.Tasty
import Test.Tasty.HUnit

days :: [Day]
days =
    [ fromGregorian 2005 February 28
    , fromGregorian 2004 February 29
    , fromGregorian 2004 January 31
    , fromGregorian 2004 December 31
    , fromGregorian 2005 July 1
    , fromGregorian 2005 April 21
    , fromGregorian 2005 June 30
    ]

increments :: [Integer]
increments = [-10, -4, -1, 0, 1, 7, 83]

adders :: [(String, Integer -> Day -> Day)]
adders =
    [ ("day", addDays)
    , ("month (clip)", addGregorianMonthsClip)
    , ("month (roll over)", addGregorianMonthsRollOver)
    , ("year (clip)", addGregorianYearsClip)
    , ("year (roll over)", addGregorianYearsRollOver)
    ]

resultDays :: [String]
resultDays = do
    (aname, adder) <- adders
    increment <- increments
    day <- days
    return
        ( (showGregorian day)
            ++ " + "
            ++ (show increment)
            ++ " * "
            ++ aname
            ++ " = "
            ++ showGregorian (adder increment day)
        )

addDaysTest :: TestTree
addDaysTest = testCase "addDays" $ assertEqual "" addDaysRef $ unlines resultDays
