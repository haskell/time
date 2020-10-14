module Main where

import Criterion.Main
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System

main :: IO ()
main = do
    getCurrentTime >>= print
    getPOSIXTime >>= print . posixSecondsToUTCTime
    getZonedTime >>= print
    ct <- getCurrentTime
    defaultMain
        [ bench "getCurrentTime" $ nfIO getCurrentTime
        , bench "getPOSIXTime" $ nfIO getPOSIXTime
        , bench "getSystemTime" $ nfIO getSystemTime
        , bench "getTimeZone" $ nfIO $ getTimeZone ct
        , bench "getCurrentTimeZone" $ nfIO getCurrentTimeZone
        , bench "getZonedTime" $ nfIO getZonedTime
        ]
