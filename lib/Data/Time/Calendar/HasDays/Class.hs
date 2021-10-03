{-# LANGUAGE Safe #-}

module Data.Time.Calendar.HasDays.Class (
    HasDays (..),
) where

import Data.Time.Calendar.Days

class HasDays t where
    -- | Returns the first 'Day' in a period of days.
    firstDay :: t -> Day

    -- | Returns the last 'Day' in a period of days.
    lastDay :: t -> Day
