{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.Days
(
	-- * Days
	DayEncoding(..)
) where

import System.Time.Clock

-- | A type that encodes a day number.
class (Eq d) => DayEncoding d where
	-- | Name the given day according to the calendar.
	encodeDay :: ModJulianDay -> d
	-- | Find out which day a given calendar day is.
	-- Behaviour for invalid representations is not defined.
	decodeDay :: d -> ModJulianDay

instance DayEncoding ModJulianDay where
	encodeDay = id
	decodeDay = id
