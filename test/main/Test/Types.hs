module Test.Types() where

import Control.DeepSeq
import Data.Data
import Data.Ix
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.Clock.System
import Data.Time.Clock.TAI

class (Typeable t, Data t, NFData t) => CheckDataInstances t
class (Typeable t, Data t, NFData t, Eq t) => CheckEqInstances t
class (Typeable t, Data t, NFData t, Eq t, Ord t) => CheckOrdInstances t
class (Typeable t, Data t, NFData t, Eq t, Ord t, Ix t, Enum t) => CheckEnumInstances t
class (Typeable t, Data t, NFData t, Eq t, Ord t, Ix t, Enum t, Bounded t) => CheckBoundedInstances t

instance CheckOrdInstances UTCTime
instance CheckOrdInstances NominalDiffTime

instance CheckEnumInstances Day
instance CheckEnumInstances DayOfWeek
instance CheckOrdInstances TimeOfDay
instance CheckOrdInstances LocalTime
instance CheckOrdInstances TimeZone
instance CheckDataInstances ZonedTime
instance CheckEqInstances CalendarDiffDays
instance CheckEqInstances CalendarDiffTime
instance CheckEnumInstances Month
instance CheckEnumInstances Quarter
instance CheckBoundedInstances QuarterOfYear

instance CheckOrdInstances SystemTime

instance CheckOrdInstances AbsoluteTime
instance CheckOrdInstances UniversalTime
