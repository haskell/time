module Test.TestUtil
  (
  module Test.TestUtil
  , module Distribution.TestSuite
  ) where

import Distribution.TestSuite

data SimpleTest = SimpleTest String Result

pure :: SimpleTest -> Test
pure (SimpleTest name result) = Test (TestInstance (return (Finished result)) name [] [] (\_ _ -> Left ""))

data IO_SimpleTest = IO_SimpleTest String (IO Result)

impure :: IO_SimpleTest -> Test
impure (IO_SimpleTest name mresult) = Test (TestInstance (fmap Finished mresult) name [] [] (\_ _ -> Left ""))

diff :: String -> String -> Result
diff s t
  = if s == t then Pass else Fail ""
