module Main (main) where

import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import System.Exit
import System.IO
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.Clock.TAI

data Test = MkTest String (IO ())

tests :: [Test]
tests =
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
