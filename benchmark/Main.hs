{-# LANGUAGE PackageImports #-}
module Main where

import  Criterion.Main
import  Data.Time.Clock
import  Data.Time.Calendar
import  Data.Time.Clock.POSIX
import  Data.Time.LocalTime
import  Data.Time.Format
import  Data.Time.Clock

import qualified "time" Data.Time.Clock       as O
import qualified "time" Data.Time.Clock.POSIX as O
import qualified "time" Data.Time.LocalTime   as O
import qualified "time" Data.Time.Format      as O
import qualified "time" Data.Time.Clock       as O

main :: IO ()
main = do
    getCurrentTime >>= print
    O.getCurrentTime >>= print
    getPOSIXTime >>= print . posixToUTCTime
    O.getPOSIXTime >>= print . O.posixSecondsToUTCTime
    getZonedTime >>= print
    O.getZonedTime >>= print

    tz <- getCurrentTimeZone
    ct <- getCurrentTime
    otz <- O.getCurrentTimeZone
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

