module Test.TestUtil
  (
  module Test.TestUtil
  , module Distribution.TestSuite
  ) where

import Distribution.TestSuite

impure :: String -> IO Result -> TestInstance
impure name mresult = TestInstance {
    run = fmap Finished mresult,
    name = name,
    tags = [],
    options = [],
    setOption = \_ _ -> Left "unsupported"
}

pure :: String -> Result -> TestInstance
pure name result = impure name (return result)

diff :: String -> String -> Result
diff s t | s == t = Pass
diff _ _ = Fail ""

finish :: IO Progress -> IO Result
finish iop = do
    progress <- iop
    case progress of
        Finished result -> return result
        Progress _ iop' -> finish iop'

concatRun :: [IO Progress] -> IO Result
concatRun [] = return Pass
concatRun (iop:iops) = do
    result <- finish iop
    case result of
        Pass -> concatRun iops
        _ -> return result

concatTestInstance :: String -> [TestInstance] -> TestInstance
concatTestInstance tname tis = impure tname (concatRun (fmap run tis))

fastTestInstanceGroup :: String -> [TestInstance] -> Test
fastTestInstanceGroup tname tis | False = testGroup tname (fmap Test tis)
fastTestInstanceGroup tname tis = Test (concatTestInstance tname tis)

