{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Clock.POSIX
(
	-- * POSIX time
	-- | This is needed by System.Time.Calendar to talk to the Unix API.
	posixDay,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds
) where

import Data.Time.Clock.UTC
