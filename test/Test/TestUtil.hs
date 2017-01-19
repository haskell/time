{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Test.TestUtil
    (
    module Test.TestUtil,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    module Test.Tasty.QuickCheck,
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

assertFailure' :: String -> IO a
assertFailure' s = do
    assertFailure s
    return undefined

assertJust :: Maybe a -> IO a
assertJust (Just a) = return a
assertJust Nothing = assertFailure' "Nothing"
