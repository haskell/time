module Test.Clock.Resolution (
    testResolutions,
) where

import Control.Concurrent
import Data.Fixed
import Data.Time.Clock
import Data.Time.Clock.TAI
import Test.Tasty
import Test.Tasty.HUnit

repeatN :: Monad m => Int -> m a -> m [a]
repeatN 0 _ = return []
repeatN n ma = do
    a <- ma
    aa <- repeatN (n - 1) ma
    return $ a : aa

gcd' :: Real a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (mod' a b)

gcdAll :: Real a => [a] -> a
gcdAll = foldr gcd' 0

testResolution :: (Show dt, Real dt) => String -> (at -> at -> dt) -> (dt, IO at) -> TestTree
testResolution name timeDiff (reportedRes, getTime) =
    testCase name $ do
        t0 <- getTime
        times0 <-
            repeatN 100 $ do
                threadDelay 0
                getTime
        times1 <-
            repeatN 100 $ -- 100us
                do
                    threadDelay 1 -- 1us
                    getTime
        times2 <-
            repeatN 100 $ -- 1ms
                do
                    threadDelay 10 -- 10us
                    getTime
        times3 <-
            repeatN 100 $ -- 10ms
                do
                    threadDelay 100 -- 100us
                    getTime
        times4 <-
            repeatN 100 $ -- 100ms
                do
                    threadDelay 1000 -- 1ms
                    getTime
        let
            times = fmap (\t -> timeDiff t t0) $ times0 ++ times1 ++ times2 ++ times3 ++ times4
            foundGrid = gcdAll times
        assertBool ("reported resolution: " <> show reportedRes <> ", found: " <> show foundGrid) $ foundGrid <= reportedRes

testResolutions :: TestTree
testResolutions =
    testGroup "resolution" $
        [testResolution "getCurrentTime" diffUTCTime (realToFrac getTime_resolution, getCurrentTime)]
            ++ case taiClock of
                Just clock -> [testResolution "taiClock" diffAbsoluteTime clock]
                Nothing -> []
