module Test.AddDiff (
    AddDiff (..),
    testAddDiff,
) where

import Test.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)

data AddDiff duration time = MkAddDiff
    { adName :: String
    , adAdd :: duration -> time -> time
    , adDifference :: time -> time -> duration
    }

testAddDiff :: (Arbitrary (NoLeapSeconds time), Eq time, Show time) => AddDiff duration time -> TestTree
testAddDiff MkAddDiff{..} =
    testProperty adName $
        \(MkNoLeapSeconds time1) (MkNoLeapSeconds time2) ->
            adAdd (adDifference time2 time1) time1 == time2
