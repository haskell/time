{-# OPTIONS -Wall -Werror #-}

-- | Types and functions for UTC and UT1
module System.Time.Clock
(
	module System.Time.Clock.Scale,
	module System.Time.Clock.UTC,
	module System.Time.Clock.Current
) where

import System.Time.Clock.Scale
import System.Time.Clock.UTC(UTCTime(..),UTCDiffTime,addUTCTime,diffUTCTime)
import System.Time.Clock.Current
