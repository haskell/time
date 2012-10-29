module Test.TestUtil
  (
  module Test.TestUtil
  , module Distribution.TestSuite
  ) where

import Distribution.TestSuite

data SimpleTest = SimpleTest String Result

pure :: SimpleTest -> TestInstance
pure (SimpleTest name result) = TestInstance (return (Finished result)) name [] [] (\_ _ -> Left "")

data IO_SimpleTest = IO_SimpleTest String (IO Result)

impure :: IO_SimpleTest -> TestInstance
impure (IO_SimpleTest name mresult) = TestInstance (fmap Finished mresult) name [] [] (\_ _ -> Left "")

diff :: String -> String -> Result
diff s t
  = if s == t then Pass else Fail ""

finish :: IO Progress -> IO Result
finish iop = do
    progress <- iop
    case progress of
        Finished result -> return result
        Progress _ iop' -> finish iop'

concatRun :: [IO Progress] -> IO Progress
concatRun [] = return (Finished Pass)
concatRun (iop:iops) = do
    result <- finish iop
    case result of
        Pass -> concatRun iops
        _ -> return (Finished result)

concatTestInstance :: String -> [TestInstance] -> TestInstance
concatTestInstance tname tis = TestInstance {
    run = concatRun (fmap run tis),
    name = tname,
    tags = [],
    options = [],
    setOption = \_ _ -> Left "unsupported"
}

fastTestInstanceGroup :: String -> [TestInstance] -> Test
--fastTestGroup tname tis = testGroup tname (fmap Test tis)
fastTestInstanceGroup tname tis = Test (concatTestInstance tname tis)

