module Main where

-------------------------------------------------------------------------------

import     Criterion.Main
import     Data.Time.Clock.POSIX
import     Data.Time

main :: IO ()
main = do
    getCurrentTime >>= print
    getPOSIXTime >>= print . posixSecondsToUTCTime
    getZonedTime >>= print
    defaultMain
        [ bgroup "time"
            [ bench "getCurrentTime" $ nfIO getCurrentTime
            , bench "getPOSIXTime" $ nfIO getPOSIXTime
            , bench "getZonedTime" $ nfIO getZonedTime
            ]
        ]

