{-# LANGUAGE PackageImports #-}
module Main where

import  Criterion.Main
import  Data.Time
import  Data.Time.Clock.POSIX

import qualified "time" Data.Time             as O
import qualified "time" Data.Time.Clock.POSIX as O

main :: IO ()
main = do
    getCurrentTime >>= print
    O.getCurrentTime >>= print
    getPOSIXTime >>= print . posixToUTCTime
    O.getPOSIXTime >>= print . O.posixSecondsToUTCTime
    getZonedTime >>= print
    O.getZonedTime >>= print

    _tz <- getCurrentTimeZone
    ct <- getCurrentTime
    _otz <- O.getCurrentTimeZone
    oct <- O.getCurrentTime

    defaultMain
        [ bgroup "new"
            [ bench "getCurrentTime" $ nfIO getCurrentTime
            , bench "getPOSIXTime" $ nfIO getPOSIXTime
            , bench "getZonedTime" $ nfIO getZonedTime
            , bench "formatTime" $ nf (formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %Z") ct
            ]
        ,
          bgroup "old"
            [ bench "getCurrentTime" $ nfIO O.getCurrentTime
            , bench "getPOSIXTime" $ nfIO O.getPOSIXTime
            , bench "getZonedTime" $ nfIO O.getZonedTime
            , bench "formatTime" $ nf (O.formatTime O.defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %Z") oct
            ]
        ]

