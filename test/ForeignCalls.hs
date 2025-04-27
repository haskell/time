module Main (main) where

import Control.Exception
import Data.Monoid
import Data.Traversable
import System.Exit
import System.IO
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System

data Test = MkTest String (IO ())

tests :: [Test]
tests =
    [ MkTest "getCurrentTime" $ getCurrentTime >> return ()
    , MkTest "getZonedTime" $ getZonedTime >> return ()
    , MkTest "getCurrentTimeZone" $ getCurrentTimeZone >> return ()
    , MkTest "getTimeZone" $ getCurrentTime >>= getTimeZone >> return ()
    , MkTest "getPOSIXTime" $ getPOSIXTime >> return ()
    , MkTest "getSystemTime" $ getSystemTime >> return ()
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
