{-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.TimeZone (
    -- * Time zones
    TimeZone (..),
    timeZoneOffsetString,
    timeZoneOffsetString',
    timeZoneOffsetString'',
    minutesToTimeZone,
    hoursToTimeZone,
    utc,
) where

import Control.DeepSeq
import Data.Data
import Data.Time.Calendar.Private
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH

-- | A TimeZone is a whole number of minutes offset from UTC, together with a name and a \"just for summer\" flag.
data TimeZone = TimeZone
    { timeZoneMinutes :: Int
    -- ^ The number of minutes offset from UTC. Positive means local time will be later in the day than UTC.
    , timeZoneSummerOnly :: Bool
    -- ^ Is this time zone just persisting for the summer?
    , timeZoneName :: String
    -- ^ The name of the zone, typically a three- or four-letter acronym.
    }
    deriving (Eq, Ord, Typeable, Data, Generic, TH.Lift)

instance NFData TimeZone where
    rnf (TimeZone m so n) = rnf m `seq` rnf so `seq` rnf n `seq` ()

-- | Create a nameless non-summer timezone for this number of minutes.
minutesToTimeZone :: Int -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours.
hoursToTimeZone :: Int -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)

showT :: Bool -> PadOption -> Int -> String
showT False opt t = showPaddedNum opt ((div t 60) * 100 + (mod t 60))
showT True opt t =
    let
        opt' = case opt of
            NoPad -> NoPad
            Pad i c -> Pad (max 0 $ i - 3) c
    in
        showPaddedNum opt' (div t 60) ++ ":" ++ show2 (mod t 60)

timeZoneOffsetString'' :: Bool -> PadOption -> TimeZone -> String
timeZoneOffsetString'' colon opt (TimeZone t _ _)
    | t < 0 = '-' : (showT colon opt (negate t))
timeZoneOffsetString'' colon opt (TimeZone t _ _) = '+' : (showT colon opt t)

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like @%z@ in formatTime), with arbitrary padding.
timeZoneOffsetString' :: Maybe Char -> TimeZone -> String
timeZoneOffsetString' Nothing = timeZoneOffsetString'' False NoPad
timeZoneOffsetString' (Just c) = timeZoneOffsetString'' False $ Pad 4 c

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like @%z@ in formatTime).
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString = timeZoneOffsetString'' False (Pad 4 '0')

-- | This only shows the time zone name, or offset if the name is empty.
instance Show TimeZone where
    show zone@(TimeZone _ _ "") = timeZoneOffsetString zone
    show (TimeZone _ _ name) = name

-- | The UTC time zone.
utc :: TimeZone
utc = TimeZone 0 False "UTC"
