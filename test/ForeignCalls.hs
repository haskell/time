{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import Data.Traversable
import System.Exit
#if defined(javascript_HOST_ARCH)
import System.Environment
#endif
import System.IO

data Test = MkTest String (IO ())

tests :: [Test]
tests =
    foreignCallTests
#if defined(javascript_HOST_ARCH)
        ++ jsTimezoneTests
#endif

foreignCallTests :: [Test]
foreignCallTests =
    [ MkTest "getCurrentTime" $ void $ getCurrentTime
    , MkTest "getZonedTime" $ void $ getZonedTime
    , MkTest "getCurrentTimeZone" $ void $ getCurrentTimeZone
    , MkTest "getTimeZone" $ void $ getCurrentTime >>= getTimeZone
    , MkTest "getPOSIXTime" $ void $ getPOSIXTime
    , MkTest "getSystemTime" $ void $ getSystemTime
    , MkTest "getTime_resolution" $ void $ evaluate getTime_resolution
    , MkTest "taiClock time" $ for_ taiClock $ \(_, getTime) -> void $ getTime
    , MkTest "taiClock resolution" $ for_ taiClock $ \(res, _) -> void $ evaluate res
    ]

#if defined(javascript_HOST_ARCH)
jsTimezoneTests :: [Test]
jsTimezoneTests =
    [ MkTest "getTimeZone JavaScript DST transition" $ do
        setEnv "TZ" "America/New_York"
        let
            transitionTime h m =
                UTCTime (fromGregorian 2024 3 10) $
                    secondsToDiffTime $
                        h * 3600 + m * 60
        zoneBefore <- getTimeZone $ transitionTime 6 30
        assertTimeZone "before DST transition" (-300) False zoneBefore
        zoneAfter <- getTimeZone $ transitionTime 7 30
        assertTimeZone "after DST transition" (-240) True zoneAfter
    ]

assertTimeZone :: String -> Int -> Bool -> TimeZone -> IO ()
assertTimeZone label expectedMinutes expectedSummer zone = do
    when (timeZoneMinutes zone /= expectedMinutes) $
        fail $
            label
                <> ": expected minutes "
                <> show expectedMinutes
                <> ", got "
                <> show (timeZoneMinutes zone)
    when (timeZoneSummerOnly zone /= expectedSummer) $
        fail $
            label
                <> ": expected summer flag "
                <> show expectedSummer
                <> ", got "
                <> show (timeZoneSummerOnly zone)
#endif

runTest :: Test -> IO Bool
runTest (MkTest name action) = do
    hPutStr stderr $ name <> ": "
    result <- try action
    case result of
        Left err -> do
            hPutStrLn stderr $ "FAILED: " <> show (err :: SomeException)
            return False
        Right () -> do
            hPutStrLn stderr "PASSED"
            return True

main :: IO ()
main = do
    results <- for tests $ \test -> do
        passed <- runTest test
        return (Sum $ if passed then 1 else 0 :: Int, Sum 1)
    let
        (Sum i, Sum n) = mconcat results
    hPutStrLn stderr $ show i <> " out of " <> show n <> " tests passed"
    exitWith $ if i == n then ExitSuccess else ExitFailure 1
