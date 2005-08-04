{-# OPTIONS -Wall -Werror #-}

-- | Types and functions for UTC and UT1
module Data.Time.Clock
(
	module Data.Time.Clock.Scale,
	module Data.Time.Clock.UTC,
	module Data.Time.Clock.Current
) where

import Data.Time.Clock.Scale
import Data.Time.Clock.UTC(UTCTime(..),UTCDiffTime,addUTCTime,diffUTCTime)
import Data.Time.Clock.Current
