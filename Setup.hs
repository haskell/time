module Main (main) where

import Control.Exception
import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.Info

main :: IO ()
main = do args <- getArgs
          let (ghcArgs, args') = extractGhcArgs args
              (_, args'') = extractConfigureArgs args'
              hooks = defaultUserHooks {
                  confHook = add_Win32_dep
                           $ confHook defaultUserHooks,
                  buildHook = add_ghc_options ghcArgs
                            $ buildHook defaultUserHooks,
                  runTests = runTestScript }
          withArgs args'' $ defaultMainWithHooks hooks

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

runTestScript :: Args -> Bool -> PackageDescription -> LocalBuildInfo
              -> IO ExitCode
runTestScript _args _flag _pd _lbi
 = withCurrentDirectory "test" (system "make")

extractGhcArgs :: [String] -> ([String], [String])
extractGhcArgs = extractPrefixArgs "--ghc-option="

extractConfigureArgs :: [String] -> ([String], [String])
extractConfigureArgs = extractPrefixArgs "--configure-option="

extractPrefixArgs :: String -> [String] -> ([String], [String])
extractPrefixArgs the_prefix args
 = let f [] = ([], [])
       f (x:xs) = case f xs of
                      (wantedArgs, otherArgs) ->
                          case removePrefix the_prefix x of
                              Just wantedArg ->
                                  (wantedArg:wantedArgs, otherArgs)
                              Nothing ->
                                  (wantedArgs, x:otherArgs)
   in f args

removePrefix :: String -> String -> Maybe String
removePrefix "" ys = Just ys
removePrefix _  "" = Nothing
removePrefix (x:xs) (y:ys)
 | x == y = removePrefix xs ys
 | otherwise = Nothing

type Hook a = PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> a
           -> IO ()

add_ghc_options :: [String] -> Hook a -> Hook a
add_ghc_options args f pd lbi muhs x
 = do let lib' = case library pd of
                     Just lib ->
                         let bi = libBuildInfo lib
                             opts = options bi ++ [(GHC, args)]
                             bi' = bi { options = opts }
                         in lib { libBuildInfo = bi' }
                     Nothing -> error "Expected a library"
          pd' = pd { library = Just lib' }
      f pd' lbi muhs x

type ConfHook = PackageDescription -> ConfigFlags -> IO LocalBuildInfo

-- XXX Hideous hack
add_Win32_dep :: ConfHook -> ConfHook
add_Win32_dep f pd cf
 = do let pd' = if os == "mingw32"
                then pd { buildDepends = Dependency "Win32" AnyVersion
                                       : buildDepends pd }
                else pd
      f pd' cf

