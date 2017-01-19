module Test.Resolution(testResolution) where

import Data.Fixed
import Data.Time.Clock
import Data.Time.Clock.TAI
import Test.TestUtil

repeatN :: Monad m => Int -> m a -> m [a]
repeatN 0 _ = return []
repeatN n ma = do
    a <- ma
    aa <- repeatN (n - 1) ma
    return $ a:aa

gcd' :: Real a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (mod' a b)

gcdAll :: Real a => [a] -> a
gcdAll = foldr gcd' 0

testClockResolution :: TestTree
testClockResolution = testCase "getCurrentTime" $ do
    times <- repeatN 100 getCurrentTime
    assertEqual "resolution" getTime_resolution $ gcdAll (fmap utctDayTime times)

testTAIResolution :: (DiffTime,IO AbsoluteTime) -> TestTree
testTAIResolution (res,getTime) = testCase "taiClock" $ do
    times <- repeatN 100 getTime
    assertEqual "resolution" res $ gcdAll (fmap (\t -> diffAbsoluteTime t taiEpoch) times)

testResolution :: TestTree
testResolution = testGroup "resolution" $
    [
    testClockResolution
    ]
    ++ case taiClock of
        Just clock -> [testTAIResolution clock]
        Nothing -> []
