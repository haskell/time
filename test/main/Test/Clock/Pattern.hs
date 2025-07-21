module Test.Clock.Pattern (
    testClockPatterns
) where

import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit

testClockPatterns :: TestTree
testClockPatterns =
    let
        testSame :: forall a. (Eq a, Show a) => TestName -> a -> a -> TestTree
        testSame name expected found = testCase name $ assertEqual "" expected found
    in
    testGroup "pattern" $
            [
            testGroup "construct"
                [ testSame "Seconds" 52.4 (Seconds 52.4)
                , testSame "Seconds-Picoseconds" (Picoseconds 7_276_000_000_000) (Seconds 7.276)
                , testSame "Minutes-Seconds" (Seconds 210) (Minutes 3.5)
                , testSame "Hours-Minutes" (Minutes 120) (Hours 2)
                , testSame "Nominal" 37.4 (Nominal 37.4)
                ],
            testGroup "deconstruct"
                [ testSame "Seconds" 52.4 ((\(Seconds x) -> x) 52.4)
                , testSame "Seconds-Picoseconds" 7.276 ((\(Seconds x) -> x) $ Picoseconds 7_276_000_000_000)
                , testSame "Minutes-Seconds" 3.5 ((\(Minutes x) -> x) $ Seconds 210)
                , testSame "Hours-Minutes" 2 ((\(Hours x) -> x) $ Minutes 120)
                , testSame "Nominal" 37.4 ((\(Nominal x) -> x) 37.4)
                ]
            ]
