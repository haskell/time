{-# LANGUAGE Safe #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Format.Parse.Instances (

) where

import Control.Applicative ((<|>))
import Data.Char
import Data.Fixed
import Data.List (elemIndex, find)
import Data.Maybe
import Data.Ratio
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private (clipValid)
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.POSIX
import Data.Time.Format.Locale
import Data.Time.Format.Parse.Class
import Data.Time.LocalTime.Internal.CalendarDiffTime
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.ZonedTime
import Data.Traversable
import Text.Read (readMaybe)

data WeekType
    = ISOWeek
    | SundayWeek
    | MondayWeek
    deriving Eq

mkDayFromWeekType :: WeekType -> Year -> WeekOfYear -> DayOfWeek -> Maybe Day
mkDayFromWeekType wt y woy dow =
    case wt of
        ISOWeek -> fromWeekDateValid y woy $ fromEnum dow
        SundayWeek -> fromSundayStartWeekValid y woy $ mod (fromEnum dow) 7
        MondayWeek -> fromMondayStartWeekValid y woy $ fromEnum dow

data DayFact
    = CenturyDayFact Integer -- century of all years
    | YearOfCenturyDayFact Integer -- 0-99, last two digits of both real years and week years
    | QuarterOfYearDayFact QuarterOfYear
    | MonthOfYearDayFact MonthOfYear -- 1-12
    | DayOfMonthDayFact DayOfMonth -- 1-31
    | DayOfYearDayFact DayOfYear -- 1-366
    | DayOfWeekDayFact DayOfWeek
    | WeekOfYearDayFact
        WeekType
        WeekOfYear -- 1-53 or 0-53
    | UTCTimeDayFact UTCTime
    | TimeZoneDayFact TimeZone

lastMatch :: (a -> Maybe b) -> [a] -> Maybe b
lastMatch f aa = listToMaybe $ reverse $ catMaybes $ fmap f aa

dayFactGetCentury :: [DayFact] -> Maybe Integer
dayFactGetCentury = lastMatch $ \case
    CenturyDayFact x -> Just x
    _ -> Nothing

dayFactGetYearOfCentury :: [DayFact] -> Maybe Integer
dayFactGetYearOfCentury = lastMatch $ \case
    YearOfCenturyDayFact x -> Just x
    _ -> Nothing

dayFactGetQuarterOfYear :: [DayFact] -> Maybe QuarterOfYear
dayFactGetQuarterOfYear = lastMatch $ \case
    QuarterOfYearDayFact x -> Just x
    _ -> Nothing

dayFactGetMonthOfYear :: [DayFact] -> Maybe MonthOfYear
dayFactGetMonthOfYear = lastMatch $ \case
    MonthOfYearDayFact x -> Just x
    _ -> Nothing

dayFactGetDayOfMonth :: [DayFact] -> Maybe DayOfMonth
dayFactGetDayOfMonth = lastMatch $ \case
    DayOfMonthDayFact x -> Just x
    _ -> Nothing

dayFactGetDayOfYear :: [DayFact] -> Maybe DayOfYear
dayFactGetDayOfYear = lastMatch $ \case
    DayOfYearDayFact x -> Just x
    _ -> Nothing

dayFactGetDayOfWeek :: [DayFact] -> Maybe DayOfWeek
dayFactGetDayOfWeek = lastMatch $ \case
    DayOfWeekDayFact x -> Just x
    _ -> Nothing

dayFactGetWeekOfYear :: [DayFact] -> Maybe (WeekType, WeekOfYear)
dayFactGetWeekOfYear = lastMatch $ \case
    WeekOfYearDayFact wt x -> Just (wt, x)
    _ -> Nothing

dayFactGetUTCTime :: [DayFact] -> Maybe UTCTime
dayFactGetUTCTime = lastMatch $ \case
    UTCTimeDayFact x -> Just x
    _ -> Nothing

dayFactGetTimeZone :: [DayFact] -> Maybe TimeZone
dayFactGetTimeZone = lastMatch $ \case
    TimeZoneDayFact x -> Just x
    _ -> Nothing

readSpec_z :: String -> Maybe Int
readSpec_z = readTzOffset

readSpec_Z :: TimeLocale -> String -> Maybe TimeZone
readSpec_Z _ str | Just offset <- readTzOffset str = Just $ TimeZone offset False ""
readSpec_Z l str | Just zone <- getKnownTimeZone l str = Just zone
readSpec_Z _ "UTC" = Just utc
readSpec_Z _ [c] | Just zone <- getMilZone c = Just zone
readSpec_Z _ _ = Nothing

makeDayFact :: TimeLocale -> Char -> String -> Maybe [DayFact]
makeDayFact l c x =
    let
        ra :: Read a => Maybe a
        ra = readMaybe x
        zeroBasedListIndex :: [String] -> Maybe Int
        zeroBasedListIndex ss = elemIndex (map toUpper x) $ fmap (map toUpper) ss
        oneBasedListIndex :: [String] -> Maybe Int
        oneBasedListIndex ss = do
            index <- zeroBasedListIndex ss
            return $ 1 + index
    in
        case c of
            -- %C: century (all but the last two digits of the year), 00 - 99
            'C' -> do
                a <- ra
                return [CenturyDayFact a]
            -- %f century (all but the last two digits of the year), 00 - 99
            'f' -> do
                a <- ra
                return [CenturyDayFact a]
            -- %Y: year
            'Y' -> do
                a <- ra
                return [CenturyDayFact (a `div` 100), YearOfCenturyDayFact (a `mod` 100)]
            -- %G: year for Week Date format
            'G' -> do
                a <- ra
                return [CenturyDayFact (a `div` 100), YearOfCenturyDayFact (a `mod` 100)]
            -- %y: last two digits of year, 00 - 99
            'y' -> do
                a <- ra
                return [YearOfCenturyDayFact a]
            -- %g: last two digits of year for Week Date format, 00 - 99
            'g' -> do
                a <- ra
                return [YearOfCenturyDayFact a]
            -- %v: quarter of year, 1 - 4
            'v' -> do
                raw <- ra
                a <- clipValid 1 4 raw
                return [QuarterOfYearDayFact $ toEnum a]
            -- %B: month name, long form (fst from months locale), January - December
            'B' -> do
                a <- oneBasedListIndex $ fmap fst $ months l
                return [MonthOfYearDayFact a]
            -- %b: month name, short form (snd from months locale), Jan - Dec
            'b' -> do
                a <- oneBasedListIndex $ fmap snd $ months l
                return [MonthOfYearDayFact a]
            -- %m: month of year, leading 0 as needed, 01 - 12
            'm' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [MonthOfYearDayFact a]
            -- %d: day of month, leading 0 as needed, 01 - 31
            'd' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [DayOfMonthDayFact a]
            -- %e: day of month, leading space as needed, 1 - 31
            'e' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [DayOfMonthDayFact a]
            -- %V: week for Week Date format, 01 - 53
            'V' -> do
                raw <- ra
                a <- clipValid 1 53 raw
                return [WeekOfYearDayFact ISOWeek a]
            -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 00 - 53
            'U' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [WeekOfYearDayFact SundayWeek a]
            -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 00 - 53
            'W' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [WeekOfYearDayFact MondayWeek a]
            -- %u: day for Week Date format, 1 - 7
            'u' -> do
                raw <- ra
                a <- clipValid 1 7 raw
                return [DayOfWeekDayFact $ toEnum a]
            -- %a: day of week, short form (snd from wDays locale), Sun - Sat
            'a' -> do
                a <- zeroBasedListIndex $ fmap snd $ wDays l
                return [DayOfWeekDayFact $ toEnum a]
            -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
            'A' -> do
                a <- zeroBasedListIndex $ fmap fst $ wDays l
                return [DayOfWeekDayFact $ toEnum a]
            -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
            'w' -> do
                raw <- ra
                a <- clipValid 0 6 raw
                return [DayOfWeekDayFact $ toEnum a]
            -- %j: day of year for Ordinal Date format, 001 - 366
            'j' -> do
                raw <- ra
                a <- clipValid 1 366 raw
                return [DayOfYearDayFact a]
            -- %s: number of whole seconds since the Unix epoch.
            's' -> do
                raw <- ra
                return [UTCTimeDayFact $ posixSecondsToUTCTime $ fromInteger raw]
            'z' -> do
                a <- readSpec_z x
                return [TimeZoneDayFact $ TimeZone a False ""]
            'Z' -> do
                a <- readSpec_Z l x
                return [TimeZoneDayFact a]
            -- unrecognised, pass on to other parsers
            _ -> return []

makeDayFacts :: TimeLocale -> [(Char, String)] -> Maybe [DayFact]
makeDayFacts l pairs = do
    factss <- for pairs $ \(c, x) -> makeDayFact l c x
    return $ mconcat factss

dayFactYear :: [DayFact] -> Integer
dayFactYear facts =
    let
        d = fromMaybe 70 $ dayFactGetYearOfCentury facts
        c =
            fromMaybe
                ( if d >= 69
                    then 19
                    else 20
                )
                $ dayFactGetCentury facts
    in
        100 * c + d

dayFactDay :: [DayFact] -> Maybe Day
dayFactDay facts =
    case dayFactYear facts of
        y | Just doy <- dayFactGetDayOfYear facts -> fromOrdinalDateValid y doy
        y
            | Just moy <- dayFactGetMonthOfYear facts ->
                let
                    dom = fromMaybe 1 $ dayFactGetDayOfMonth facts
                in
                    fromGregorianValid y moy dom
        y
            | Just (wt, woy) <- dayFactGetWeekOfYear facts ->
                let
                    dow = fromMaybe Thursday $ dayFactGetDayOfWeek facts
                in
                    mkDayFromWeekType wt y woy dow
        y
            | Just qoy <- dayFactGetQuarterOfYear facts ->
                let
                    moy = case qoy of
                        Q1 -> 1
                        Q2 -> 4
                        Q3 -> 7
                        Q4 -> 10
                    dom = fromMaybe 1 $ dayFactGetDayOfMonth facts
                in
                    fromGregorianValid y moy dom
        _
            | Just ut <- dayFactGetUTCTime facts ->
                let
                    tz = fromMaybe utc $ dayFactGetTimeZone facts
                in
                    Just $ localDay $ utcToLocalTime tz ut
        y | Just dom <- dayFactGetDayOfMonth facts -> fromGregorianValid y 1 dom
        y | Just dow <- dayFactGetDayOfWeek facts -> fromWeekDateValid y 1 $ fromEnum dow
        y -> fromOrdinalDateValid y 1

instance ParseTime Day where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeDayFacts l pairs
        dayFactDay facts

instance ParseTime DayOfWeek where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeDayFacts l pairs
        dayFactGetDayOfWeek facts
            <|> (fmap dayOfWeek $ dayFactDay facts)

instance ParseTime Month where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeDayFacts l pairs
        case dayFactGetMonthOfYear facts of
            Just moy -> let
                    y = dayFactYear facts
                in Just $ YearMonth y moy
            Nothing -> fmap dayPeriod $ dayFactDay facts

instance ParseTime QuarterOfYear where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeDayFacts l pairs
        case dayFactGetQuarterOfYear facts of
            Just qoy -> Just qoy
            Nothing -> do
                QuarterDay (YearQuarter _ qoy) _ <- dayFactDay facts
                return qoy

instance ParseTime Quarter where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeDayFacts l pairs
        case dayFactGetQuarterOfYear facts of
            Just qoy -> let
                    y = dayFactYear facts
                in Just $ YearQuarter y qoy
            Nothing -> fmap dayPeriod $ dayFactDay facts

mfoldl :: Monad m => (a -> b -> m a) -> m a -> [b] -> m a
mfoldl f =
    let
        mf ma b = do
            a <- ma
            f a b
    in
        foldl mf

data AMPM = AM | PM

data TimeFact
    = AMAPMTimeFact AMPM
    | HourTimeFact Int
    | MinuteTimeFact Int
    | WholeSecondTimeFact Int
    | FractSecondTimeFact Pico
    | UTCTimeFact UTCTime
    | ZoneTimeFact TimeZone

timeFactGetAMPM :: [TimeFact] -> Maybe AMPM
timeFactGetAMPM = lastMatch $ \case
    AMAPMTimeFact x -> Just x
    _ -> Nothing

timeFactGetHour :: [TimeFact] -> Maybe Int
timeFactGetHour = lastMatch $ \case
    HourTimeFact x -> Just x
    _ -> Nothing

timeFactGetMinute :: [TimeFact] -> Maybe Int
timeFactGetMinute = lastMatch $ \case
    MinuteTimeFact x -> Just x
    _ -> Nothing

timeFactGetWholeSecond :: [TimeFact] -> Maybe Int
timeFactGetWholeSecond = lastMatch $ \case
    WholeSecondTimeFact x -> Just x
    _ -> Nothing

timeFactGetFractSecond :: [TimeFact] -> Maybe Pico
timeFactGetFractSecond = lastMatch $ \case
    FractSecondTimeFact x -> Just x
    _ -> Nothing

timeFactGetUTC :: [TimeFact] -> Maybe UTCTime
timeFactGetUTC = lastMatch $ \case
    UTCTimeFact x -> Just x
    _ -> Nothing

timeFactGetZone :: [TimeFact] -> Maybe TimeZone
timeFactGetZone = lastMatch $ \case
    ZoneTimeFact x -> Just x
    _ -> Nothing

makeTimeFact :: TimeLocale -> Char -> String -> Maybe [TimeFact]
makeTimeFact l c x =
    let
        ra :: Read a => Maybe a
        ra = readMaybe x
        getAmPm =
            let
                upx = map toUpper x
                (amStr, pmStr) = amPm l
            in
                if upx == amStr
                    then Just [AMAPMTimeFact AM]
                    else
                        if upx == pmStr
                            then Just [AMAPMTimeFact PM]
                            else Nothing
    in
        case c of
            'P' -> getAmPm
            'p' -> getAmPm
            'H' -> do
                raw <- ra
                a <- clipValid 0 23 raw
                return [HourTimeFact a]
            'I' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [HourTimeFact a]
            'k' -> do
                raw <- ra
                a <- clipValid 0 23 raw
                return [HourTimeFact a]
            'l' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [HourTimeFact a]
            'M' -> do
                raw <- ra
                a <- clipValid 0 59 raw
                return [MinuteTimeFact a]
            'S' -> do
                raw <- ra
                a <- clipValid 0 60 raw
                return [WholeSecondTimeFact $ fromInteger a]
            'q' -> do
                ps <- (readMaybe $ take 12 $ rpad 12 '0' x) <|> return 0
                return [FractSecondTimeFact $ mkPico 0 ps]
            'Q' ->
                if null x
                    then return []
                    else do
                        ps <- (readMaybe $ take 12 $ rpad 12 '0' x) <|> return 0
                        return [FractSecondTimeFact $ mkPico 0 ps]
            's' -> do
                raw <- ra
                return [UTCTimeFact $ posixSecondsToUTCTime $ fromInteger raw]
            'z' -> do
                a <- readSpec_z x
                return [ZoneTimeFact $ TimeZone a False ""]
            'Z' -> do
                a <- readSpec_Z l x
                return [ZoneTimeFact a]
            _ -> return []

makeTimeFacts :: TimeLocale -> [(Char, String)] -> Maybe [TimeFact]
makeTimeFacts l pairs = do
    factss <- for pairs $ \(c, x) -> makeTimeFact l c x
    return $ mconcat factss

instance ParseTime TimeOfDay where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        facts <- makeTimeFacts l pairs
        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        case timeFactGetUTC facts of
            Just t ->
                let
                    zone = fromMaybe utc $ timeFactGetZone facts
                    sf = fromMaybe 0 $ timeFactGetFractSecond facts
                    TimeOfDay h m s = localTimeOfDay $ utcToLocalTime zone t
                in
                    return $ TimeOfDay h m $ s + sf
            Nothing ->
                let
                    h = fromMaybe 0 $ timeFactGetHour facts
                    m = fromMaybe 0 $ timeFactGetMinute facts
                    si = fromMaybe 0 $ timeFactGetWholeSecond facts
                    sf = fromMaybe 0 $ timeFactGetFractSecond facts
                    s :: Pico
                    s = fromIntegral si + sf
                    h' = case timeFactGetAMPM facts of
                        Nothing -> h
                        Just AM -> mod h 12
                        Just PM -> if h < 12 then h + 12 else h
                in
                    return $ TimeOfDay h' m s

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = LocalTime <$> (buildTime l xs) <*> (buildTime l xs)

enumDiff :: Enum a => a -> a -> Int
enumDiff a b = (fromEnum a) - (fromEnum b)

getMilZoneHours :: Char -> Maybe Int
getMilZoneHours c
    | c < 'A' = Nothing
getMilZoneHours c
    | c <= 'I' = Just $ 1 + enumDiff c 'A'
getMilZoneHours 'J' = Nothing
getMilZoneHours c
    | c <= 'M' = Just $ 10 + enumDiff c 'K'
getMilZoneHours c
    | c <= 'Y' = Just $ (enumDiff 'N' c) - 1
getMilZoneHours 'Z' = Just 0
getMilZoneHours _ = Nothing

getMilZone :: Char -> Maybe TimeZone
getMilZone c =
    let
        yc = toUpper c
    in
        do
            hours <- getMilZoneHours yc
            return $ TimeZone (hours * 60) False [yc]

getKnownTimeZone :: TimeLocale -> String -> Maybe TimeZone
getKnownTimeZone locale x = find (\tz -> map toUpper x == timeZoneName tz) (knownTimeZones locale)

instance ParseTime TimeZone where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l =
        let
            f :: Char -> String -> TimeZone -> Maybe TimeZone
            f 'z' str (TimeZone _ dst name) = do
                offset <- readSpec_z str
                return $ TimeZone offset dst name
            f 'Z' str _ = readSpec_Z l str
            f _ _ tz = Just tz
        in
            foldl (\mt (c, s) -> mt >>= f c s) (Just $ minutesToTimeZone 0)

readTzOffset :: String -> Maybe Int
readTzOffset str =
    let
        getSign '+' = Just 1
        getSign '-' = Just (-1)
        getSign _ = Nothing
        calc s h1 h2 m1 m2 = do
            sign <- getSign s
            h <- readMaybe [h1, h2]
            m <- readMaybe [m1, m2]
            return $ sign * (60 * h + m)
    in
        case str of
            (s : h1 : h2 : ':' : m1 : m2 : []) -> calc s h1 h2 m1 m2
            (s : h1 : h2 : m1 : m2 : []) -> calc s h1 h2 m1 m2
            _ -> Nothing

instance ParseTime ZonedTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs =
        let
            f (ZonedTime (LocalTime _ tod) z) ('s', x) = do
                a <- readMaybe x
                let
                    s = fromInteger a
                    (_, ps) = properFraction (todSec tod) :: (Integer, Pico)
                    s' = s + fromRational (toRational ps)
                return $ utcToZonedTime z (posixSecondsToUTCTime s')
            f t _ = Just t
        in
            mfoldl f (ZonedTime <$> (buildTime l xs) <*> (buildTime l xs)) xs

instance ParseTime UTCTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = zonedTimeToUTC <$> buildTime l xs

instance ParseTime UniversalTime where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = localTimeToUT1 0 <$> buildTime l xs

--- Duration

buildTimeMonths :: [(Char, String)] -> Maybe Integer
buildTimeMonths xs = do
    tt <-
        for xs $ \(c, s) ->
            case c of
                'y' -> fmap ((*) 12) $ readMaybe s
                'b' -> readMaybe s
                'B' -> readMaybe s
                _ -> return 0
    return $ sum tt

buildTimeDays :: [(Char, String)] -> Maybe Integer
buildTimeDays xs = do
    tt <-
        for xs $ \(c, s) ->
            case c of
                'w' -> fmap ((*) 7) $ readMaybe s
                'd' -> readMaybe s
                'D' -> readMaybe s
                _ -> return 0
    return $ sum tt

buildTimeSeconds :: [(Char, String)] -> Maybe Pico
buildTimeSeconds xs = do
    tt <- for xs $ \(c, s) ->
        let
            readInt :: Integer -> Maybe Pico
            readInt t = do
                i <- readMaybe s
                return $ fromInteger $ i * t
        in
            case c of
                'h' -> readInt 3600
                'H' -> readInt 3600
                'm' -> readInt 60
                'M' -> readInt 60
                's' -> readMaybe s
                'S' -> readMaybe s
                _ -> return 0
    return $ sum tt

instance ParseTime NominalDiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        dd <- buildTimeDays xs
        tt <- buildTimeSeconds xs
        return $ (fromInteger dd * 86400) + realToFrac tt

instance ParseTime DiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        dd <- buildTimeDays xs
        tt <- buildTimeSeconds xs
        return $ (fromInteger dd * 86400) + realToFrac tt

instance ParseTime CalendarDiffDays where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime _ xs = do
        mm <- buildTimeMonths xs
        dd <- buildTimeDays xs
        return $ CalendarDiffDays mm dd

instance ParseTime CalendarDiffTime where
    parseTimeSpecifier _ = durationParseTimeSpecifier
    buildTime locale xs = do
        mm <- buildTimeMonths xs
        tt <- buildTime locale xs
        return $ CalendarDiffTime mm tt
