module Main where

-------------------------------------------------------------------------------

import     Criterion.Main
import     Data.Time.Clock.POSIX
import     Data.Time

main :: IO ()
main = do
    getCurrentTime >>= print
    getPOSIXTime >>= print . posixSecondsToUTCTime
    defaultMain
        [ bgroup "time"
            [ bench "UTCTime" $ whnfIO getCurrentTime
            , bench "POSIXTime" $ whnfIO getPOSIXTime
            ]
        ]

