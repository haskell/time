module Data.Time.Calendar.Class (
    HasDays (..),
) where

import Data.Time.Calendar.Days

class HasDays t where
    firstDay :: t -> Day
    lastDay :: t -> Day
