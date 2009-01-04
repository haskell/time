module Main (main) where

import Control.Exception
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Cmd
import System.Directory

main :: IO ()
main = do let hooks = autoconfUserHooks { runTests = runTestScript }
          defaultMainWithHooks hooks

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

runTestScript :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTestScript _args _flag _pd _lbi
 = maybeExit $ withCurrentDirectory "test" $ system "make"
