{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time

main :: IO ()
main = do
	zone <- getCurrentTimezone
	putStrLn (timezoneOffsetString zone)
