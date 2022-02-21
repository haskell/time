module Main (main) where

import Data.Time

main :: IO ()
main = do
    now <- getZonedTime
    putStrLn $ show now
