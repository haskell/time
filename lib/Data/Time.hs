{-|

= Quick Start

Use these types for time regardless of location (not caring about leap-seconds):

* 'UTCTime' for actual times
* 'NominalDiffTime' for differences between times, i.e. durations

Use these types for the ways people refer to time:

* 'Day' for something like June 27th 2017
* 'TimeOfDay' for something like 5pm
* 'LocalTime' for a 'Day' with a 'TimeOfDay'
* 'TimeZone' for a time zone offset (not actually the time zone itself) like -0700
* 'ZonedTime' for a 'LocalTime' with a 'TimeZone'

Use this for low-latency timing:

* 'Data.Time.Clock.System.SystemTime'

These are less commonly needed:

* 'Data.Time.Clock.TAI.AbsoluteTime' and 'DiffTime' if you do care about leap-seconds.
* 'Data.Time.Clock.TAI.LeapSecondMap' for tracking the leap-seconds
* 'UniversalTime' for time based on Earth rotation
-}
module Data.Time
(
    module Data.Time.Calendar,
    module Data.Time.Clock,
    module Data.Time.LocalTime,
    module Data.Time.Format
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
