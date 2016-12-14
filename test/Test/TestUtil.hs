{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Test.TestUtil
    (
    module Test.TestUtil,
    module Test.Framework,
    module Test.Framework.Providers.QuickCheck2
    ) where

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.QuickCheck2
import Data.Typeable

data Result = Pass | Fail String deriving Typeable

instance Show Result where
    show Pass = "passed"
    show (Fail s) = "failed: " ++ s

instance TestResultlike () Result where
    testSucceeded Pass = True
    testSucceeded (Fail _) = False

instance Testlike () Result (IO Result) where
    testTypeName _ = "Cases"
    runTest _ ior = do
        r <- ior
        return (Finished r,return ())

ioTest :: String -> IO Result -> Test
ioTest = Test

pureTest :: String -> Result -> Test
pureTest name result = ioTest name (return result)

diff :: (Show a,Eq a) => a -> a -> Result
diff expected found | expected == found = Pass
diff expected found = Fail ("expected " ++ (show expected) ++ " but found " ++ (show found))


-- for tasty-like test code

type TestTree = Test
type Assertion = Either String ()

assertionResult :: Assertion -> Result
assertionResult (Right ()) = Pass
assertionResult (Left s) = Fail s

testCase :: String -> Assertion -> Test
testCase name ass = pureTest name $ assertionResult ass

assertFailure :: String -> Either String a
assertFailure = Left

assertEqual :: (Show a,Eq a) => String -> a -> a -> Assertion
assertEqual _ expected found | expected == found = return ()
assertEqual name expected found = assertFailure $ name ++ ": expected " ++ (show expected) ++ " but found " ++ (show found)

assertJust :: Maybe a -> Either String a
assertJust (Just a) = return a
assertJust Nothing = assertFailure "Nothing"
