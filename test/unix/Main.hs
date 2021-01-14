module Main where

import Test.Format.Format
import Test.LocalTime.TimeZone
import Test.Tasty

tests :: TestTree
tests = testGroup "Time" [testGroup "Format" testFormat, testGroup "LocalTime" [testTimeZone]]

main :: IO ()
main = defaultMain tests
