module Main where

import Data.Time

showTZ :: TimeZone -> String
showTZ tz = (formatTime defaultTimeLocale "%Z %z " tz) ++ show (timeZoneSummerOnly tz)

main :: IO ()
main = mapM_ (\tz -> putStrLn (showTZ tz)) (knownTimeZones defaultTimeLocale)
