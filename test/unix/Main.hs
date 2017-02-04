module Main where

import Foreign.C.Types
import Test.Tasty
import Test.Format.Format
import Test.LocalTime.TimeZone


tests :: TestTree
tests = testGroup "Time" [
    testGroup "Format" [
        testFormat
        ],
    testGroup "LocalTime" [
        testTimeZone
        ]
    ]

main :: IO ()
main = do
  if (toRational (1000000000000 :: CTime)) /= (1000000000000 :: Rational)
    then putStrLn "WARNING: Some tests will incorrectly fail due to a 32-bit time_t C type."
    else return ()
  defaultMain tests
