{-# LANGUAGE Safe #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Format.Format.Instances (

) where

import Control.Applicative ((<|>))
import Data.Char
import Data.Fixed
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private
import Data.Time.Calendar.Week
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.POSIX
import Data.Time.Format.Format.Class
import Data.Time.Format.Locale
import Data.Time.LocalTime.Internal.CalendarDiffTime
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.ZonedTime

mapFormatCharacter :: (b -> a) -> Maybe (FormatOptions -> a -> String) -> Maybe (FormatOptions -> b -> String)
mapFormatCharacter ba = fmap $ fmap $ \as -> as . ba

instance FormatTime LocalTime where
    formatCharacter _ 'c' = Just $ \fo -> formatTime (foLocale fo) $ dateTimeFmt $ foLocale fo
    formatCharacter alt c =
        mapFormatCharacter localDay (formatCharacter alt c)
            <|> mapFormatCharacter localTimeOfDay (formatCharacter alt c)

todAMPM :: TimeLocale -> TimeOfDay -> String
todAMPM locale day =
    let
        (am, pm) = amPm locale
    in
        if (todHour day) < 12
            then am
            else pm

tod12Hour :: TimeOfDay -> Int
tod12Hour day = (mod (todHour day - 1) 12) + 1

instance FormatTime TimeOfDay where
    -- Aggregate
    formatCharacter _ 'R' = Just $ formatString $ \locale -> formatTime locale "%H:%M"
    formatCharacter _ 'T' = Just $ formatString $ \locale -> formatTime locale "%H:%M:%S"
    formatCharacter _ 'X' = Just $ formatString $ \locale -> formatTime locale (timeFmt locale)
    formatCharacter _ 'r' = Just $ formatString $ \locale -> formatTime locale (time12Fmt locale)
    -- AM/PM
    formatCharacter _ 'P' = Just $ formatString $ \locale -> map toLower . todAMPM locale
    formatCharacter _ 'p' = Just $ formatString $ \locale -> todAMPM locale
    -- Hour
    formatCharacter _ 'H' = Just $ formatNumber True 2 '0' todHour
    formatCharacter _ 'I' = Just $ formatNumber True 2 '0' tod12Hour
    formatCharacter _ 'k' = Just $ formatNumber True 2 ' ' todHour
    formatCharacter _ 'l' = Just $ formatNumber True 2 ' ' tod12Hour
    -- Minute
    formatCharacter _ 'M' = Just $ formatNumber True 2 '0' todMin
    -- Second
    formatCharacter _ 'S' = Just $ formatNumber True 2 '0' $ (floor . todSec :: TimeOfDay -> Int)
    formatCharacter _ 'q' = Just $ formatGeneral True True 12 '0' $ \_ pado -> showPaddedFixedFraction pado . todSec
    formatCharacter _ 'Q' =
        Just $ formatGeneral True False 12 '0' $ \_ pado -> dotNonEmpty . showPaddedFixedFraction pado . todSec
      where
        dotNonEmpty "" = ""
        dotNonEmpty s = '.' : s
    -- Default
    formatCharacter _ _ = Nothing

instance FormatTime ZonedTime where
    formatCharacter _ 'c' = Just $ formatString $ \locale -> formatTime locale (dateTimeFmt locale)
    formatCharacter _ 's' =
        Just $ formatNumber True 1 '0' $ (floor . utcTimeToPOSIXSeconds . zonedTimeToUTC :: ZonedTime -> Integer)
    formatCharacter alt c =
        mapFormatCharacter zonedTimeToLocalTime (formatCharacter alt c)
            <|> mapFormatCharacter zonedTimeZone (formatCharacter alt c)

instance FormatTime TimeZone where
    formatCharacter False 'z' = Just $ formatGeneral False True 4 '0' $ \_ -> timeZoneOffsetString'' False
    formatCharacter True 'z' = Just $ formatGeneral False True 5 '0' $ \_ -> timeZoneOffsetString'' True
    formatCharacter alt 'Z' =
        Just $ \fo z ->
            let
                n = timeZoneName z
                idef =
                    if alt
                        then 5
                        else 4
            in
                if null n
                    then formatGeneral False True idef '0' (\_ -> timeZoneOffsetString'' alt) fo z
                    else formatString (\_ -> timeZoneName) fo z
    formatCharacter _ _ = Nothing

instance FormatTime DayOfWeek where
    formatCharacter _ 'u' = Just $ formatNumber True 1 '0' $ fromEnum
    formatCharacter _ 'w' = Just $ formatNumber True 1 '0' $ \wd -> (mod (fromEnum wd) 7)
    formatCharacter _ 'a' = Just $ formatString $ \locale wd -> snd $ (wDays locale) !! (mod (fromEnum wd) 7)
    formatCharacter _ 'A' = Just $ formatString $ \locale wd -> fst $ (wDays locale) !! (mod (fromEnum wd) 7)
    formatCharacter _ _ = Nothing

instance FormatTime Month where
    -- Year Count
    formatCharacter _ 'Y' = Just $ formatNumber False 4 '0' $ \(YearMonth y _) -> y
    formatCharacter _ 'y' = Just $ formatNumber True 2 '0' $ \(YearMonth y _) -> mod100 y
    formatCharacter _ 'C' = Just $ formatNumber False 2 '0' $ \(YearMonth y _) -> div100 y
    -- Month of Year
    formatCharacter _ 'B' =
        Just $ formatString $ \locale (YearMonth _ my) -> fst $ (months locale) !! (my - 1)
    formatCharacter _ 'b' =
        Just $ formatString $ \locale (YearMonth _ my) -> snd $ (months locale) !! (my - 1)
    formatCharacter _ 'h' =
        Just $ formatString $ \locale (YearMonth _ my) -> snd $ (months locale) !! (my - 1)
    formatCharacter _ 'm' = Just $ formatNumber True 2 '0' $ \(YearMonth _ m) -> m
    -- Default
    formatCharacter _ _ = Nothing

instance FormatTime Day where
    -- Aggregate
    formatCharacter _ 'D' = Just $ formatString $ \locale -> formatTime locale "%m/%d/%y"
    formatCharacter _ 'F' = Just $ formatString $ \locale -> formatTime locale "%Y-%m-%d"
    formatCharacter _ 'x' = Just $ formatString $ \locale -> formatTime locale (dateFmt locale)
    -- Day of Month
    formatCharacter _ 'd' = Just $ formatNumber True 2 '0' $ \(YearMonthDay _ _ dm) -> dm
    formatCharacter _ 'e' = Just $ formatNumber True 2 ' ' $ \(YearMonthDay _ _ dm) -> dm
    -- Day of Year
    formatCharacter _ 'j' = Just $ formatNumber True 3 '0' $ \(YearDay _ dy) -> dy
    -- ISO 8601 Week Date
    formatCharacter _ 'G' = Just $ formatNumber False 4 '0' $ \(YearWeekDay y _ _) -> y
    formatCharacter _ 'g' = Just $ formatNumber True 2 '0' $ \(YearWeekDay y _ _) -> mod100 y
    formatCharacter _ 'f' = Just $ formatNumber False 2 '0' $ \(YearWeekDay y _ _) -> div100 y
    formatCharacter _ 'V' = Just $ formatNumber True 2 '0' $ \(YearWeekDay _ wy _) -> wy
    formatCharacter _ 'u' = Just $ formatNumber True 1 '0' $ \(YearWeekDay _ _ dw) -> fromEnum dw
    -- Day of week
    formatCharacter _ 'a' = Just $ formatString $ \locale -> snd . ((wDays locale) !!) . snd . sundayStartWeek
    formatCharacter _ 'A' = Just $ formatString $ \locale -> fst . ((wDays locale) !!) . snd . sundayStartWeek
    formatCharacter _ 'U' = Just $ formatNumber True 2 '0' $ fst . sundayStartWeek
    formatCharacter _ 'w' = Just $ formatNumber True 1 '0' $ snd . sundayStartWeek
    formatCharacter _ 'W' = Just $ formatNumber True 2 '0' $ fst . mondayStartWeek
    -- Default
    formatCharacter alt c = mapFormatCharacter (\(MonthDay m _) -> m) $ formatCharacter alt c

instance FormatTime UTCTime where
    formatCharacter alt c = mapFormatCharacter (utcToZonedTime utc) $ formatCharacter alt c

instance FormatTime UniversalTime where
    formatCharacter alt c = mapFormatCharacter (ut1ToLocalTime 0) $ formatCharacter alt c

instance FormatTime NominalDiffTime where
    formatCharacter _ 'w' = Just $ formatNumberStd 1 $ quotBy $ 7 * 86400
    formatCharacter _ 'd' = Just $ formatNumberStd 1 $ quotBy 86400
    formatCharacter _ 'D' = Just $ formatNumberStd 1 $ remBy 7 . quotBy 86400
    formatCharacter _ 'h' = Just $ formatNumberStd 1 $ quotBy 3600
    formatCharacter _ 'H' = Just $ formatNumberStd 2 $ remBy 24 . quotBy 3600
    formatCharacter _ 'm' = Just $ formatNumberStd 1 $ quotBy 60
    formatCharacter _ 'M' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 60
    formatCharacter False 's' = Just $ formatNumberStd 1 $ quotBy 1
    formatCharacter True 's' =
        Just $ formatGeneral True False 12 '0' $ \_ padf t -> showPaddedFixed NoPad padf (realToFrac t :: Pico)
    formatCharacter False 'S' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 1
    formatCharacter True 'S' =
        Just $
            formatGeneral True False 12 '0' $ \_ padf t ->
                let
                    padn = case padf of
                        NoPad -> NoPad
                        Pad _ c -> Pad 2 c
                in
                    showPaddedFixed padn padf (realToFrac $ remBy 60 t :: Pico)
    formatCharacter _ _ = Nothing

instance FormatTime DiffTime where
    formatCharacter _ 'w' = Just $ formatNumberStd 1 $ quotBy $ 7 * 86400
    formatCharacter _ 'd' = Just $ formatNumberStd 1 $ quotBy 86400
    formatCharacter _ 'D' = Just $ formatNumberStd 1 $ remBy 7 . quotBy 86400
    formatCharacter _ 'h' = Just $ formatNumberStd 1 $ quotBy 3600
    formatCharacter _ 'H' = Just $ formatNumberStd 2 $ remBy 24 . quotBy 3600
    formatCharacter _ 'm' = Just $ formatNumberStd 1 $ quotBy 60
    formatCharacter _ 'M' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 60
    formatCharacter False 's' = Just $ formatNumberStd 1 $ quotBy 1
    formatCharacter True 's' =
        Just $ formatGeneral True False 12 '0' $ \_ padf t -> showPaddedFixed NoPad padf (realToFrac t :: Pico)
    formatCharacter False 'S' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 1
    formatCharacter True 'S' =
        Just $
            formatGeneral True False 12 '0' $ \_ padf t ->
                let
                    padn = case padf of
                        NoPad -> NoPad
                        Pad _ c -> Pad 2 c
                in
                    showPaddedFixed padn padf (realToFrac $ remBy 60 t :: Pico)
    formatCharacter _ _ = Nothing

instance FormatTime CalendarDiffDays where
    formatCharacter _ 'y' = Just $ formatNumberStd 1 $ quotBy 12 . cdMonths
    formatCharacter _ 'b' = Just $ formatNumberStd 1 $ cdMonths
    formatCharacter _ 'B' = Just $ formatNumberStd 2 $ remBy 12 . cdMonths
    formatCharacter _ 'w' = Just $ formatNumberStd 1 $ quotBy 7 . cdDays
    formatCharacter _ 'd' = Just $ formatNumberStd 1 $ cdDays
    formatCharacter _ 'D' = Just $ formatNumberStd 1 $ remBy 7 . cdDays
    formatCharacter _ _ = Nothing

instance FormatTime CalendarDiffTime where
    formatCharacter _ 'y' = Just $ formatNumberStd 1 $ quotBy 12 . ctMonths
    formatCharacter _ 'b' = Just $ formatNumberStd 1 $ ctMonths
    formatCharacter _ 'B' = Just $ formatNumberStd 2 $ remBy 12 . ctMonths
    formatCharacter alt c = mapFormatCharacter ctTime $ formatCharacter alt c
