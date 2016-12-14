module Test.Resolution(testResolution) where

import Data.Fixed
import Data.Time.Clock
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

testClockResolution = ioTest "getCurrentTime" $ do
    times <- repeatN 100 getCurrentTime
    return $ assertionResult $ assertEqual "resolution" clockResolution $ gcdAll (fmap utctDayTime times)

testResolution :: Test
testResolution = testGroup "resolution"
    [
    testClockResolution
    ]
