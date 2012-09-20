module Test.TestUtil
  ( SimpleTest(..)
  , IO_SimpleTest(..)
  , diff
  , module Distribution.TestSuite )
 where

import Distribution.TestSuite

import System.Cmd
import System.Exit

--

data SimpleTest = SimpleTest String Result

instance TestOptions SimpleTest where
  name (SimpleTest s _) = s
  options = const []
  defaultOptions _ = return $ Options []
  check _ _ = []
  
instance PureTestable SimpleTest where
  run (SimpleTest _ r) _ = r

--

data IO_SimpleTest = IO_SimpleTest String (IO Result)

instance TestOptions IO_SimpleTest where
  name (IO_SimpleTest s _) = s
  options = const []
  defaultOptions _ = return $ Options []
  check _ _ = []

instance ImpureTestable IO_SimpleTest where
  runM (IO_SimpleTest _ r) _ = r

--

diff :: String -> String -> Result
diff s t
  = if s == t then Pass else Fail ""
