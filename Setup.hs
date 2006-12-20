module Main (main) where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Exit
import System.Cmd
import System.Directory
import Control.Exception

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
	cur <- getCurrentDirectory
	setCurrentDirectory path
	finally f (setCurrentDirectory cur)

runTestScript :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode
runTestScript args flag pd lbi = withCurrentDirectory "test" (system "make")

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{runTests = runTestScript}
