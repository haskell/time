{-# LANGUAGE CPP #-}

module Test.TastyWrapper where

#if defined(javascript_HOST_ARCH)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (IOMode (ReadMode), stdin, withFile)
#endif

tastyWrapper :: IO a -> IO a
#if defined(javascript_HOST_ARCH)
-- Tasty's console reporter queries terminal width through ansi-terminal, which
-- trips h$fdReady in the JS RTS when this test runs attached to a TTY.
tastyWrapper action =
    withFile "/dev/null" ReadMode $ \devNull -> do
        hDuplicateTo devNull stdin
        action
#else
tastyWrapper = id
#endif
