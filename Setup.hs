module Main (main) where

import Distribution.Simple
import System.Info

main :: IO ()
main = case os of
    "windows" -> defaultMain
    "mingw32" -> defaultMain
    _ -> defaultMainWithHooks autoconfUserHooks
