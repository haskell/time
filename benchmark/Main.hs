module Main where


import     Criterion.Main
import     Data.Time.Clock
import     Data.Time.Clock.POSIX
import     Data.Time.LocalTime

main :: IO ()
main = do
    getCurrentTime >>= print
    getPOSIXTime >>= print . posixSecondsToUTCTime
    getZonedTime >>= print
    tz <- getCurrentTimeZone
    defaultMain
        [ bgroup "time"
            [ bench "getCurrentTime" $ nfIO getCurrentTime
            , bench "getPOSIXTime" $ nfIO getPOSIXTime
            , bench "getZonedTime" $ nfIO getZonedTime
            ]
        ]

