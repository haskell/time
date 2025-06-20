{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Format.Parse.Instances (

) where

import Control.Applicative ((<|>))
import Data.Char
import Data.Fixed
import Data.Kind
import Data.List (elemIndex, find)
import Data.Maybe
import Data.Ratio
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private (clipValid)
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
#ifdef __MHS__
import Data.Type.Equality ((:~:)(..))
#else
import Data.Type.Equality ((:~:)(..), TestEquality (..))
#endif


#ifdef __MHS__
-- MicroHs doesn't allow this to be polykinded
class TestEquality (f :: Type -> Type) where
    testEquality :: forall (a :: Type) (b :: Type). f a -> f b -> Maybe (a :~: b)
#endif

data SomeOf (w :: Type -> Type) = forall (a :: Type). MkSomeOf (w a) a
{-
getSomeOf :: forall w a. TestEquality w => SomeOf w -> w a -> Maybe a
getSomeOf (MkSomeOf w1 a) w2 = do
    Refl <- testEquality w1 w2
    return a

getSomeOfs :: forall w a. TestEquality w => [SomeOf w] -> w a -> Maybe a
getSomeOfs ss wa = listToMaybe $ catMaybes $ fmap (\s -> getSomeOf s wa) ss
-}
data WeekType
    = ISOWeek
    | SundayWeek
    | MondayWeek
    deriving Eq

data DayQuery (t :: Type) where
    CenturyDayQuery :: DayQuery Integer -- century of all years
    YearOfCenturyDayQuery :: DayQuery Integer -- 0-99, last two digits of both real years and week years
    MonthOfYearDayQuery :: DayQuery MonthOfYear -- 1-12
    DayOfMonthDayQuery :: DayQuery DayOfMonth -- 1-31
    DayOfYearDayQuery :: DayQuery DayOfYear -- 1-366
    DayOfWeekDayQuery :: DayQuery DayOfWeek
    WeekOfYearDayQuery :: WeekType -> DayQuery WeekOfYear -- 1-53 or 0-53
    UTCTimeDayQuery :: DayQuery UTCTime
    TimeZoneDayQuery :: DayQuery TimeZone

instance TestEquality DayQuery where
    testEquality CenturyDayQuery CenturyDayQuery = Just Refl
    testEquality YearOfCenturyDayQuery YearOfCenturyDayQuery = Just Refl
    testEquality MonthOfYearDayQuery MonthOfYearDayQuery = Just Refl
    testEquality DayOfMonthDayQuery DayOfMonthDayQuery = Just Refl
    testEquality DayOfYearDayQuery DayOfYearDayQuery = Just Refl
    testEquality DayOfWeekDayQuery DayOfWeekDayQuery = Just Refl
    testEquality (WeekOfYearDayQuery wa) (WeekOfYearDayQuery wb) | wa == wb = Just Refl
    testEquality UTCTimeDayQuery UTCTimeDayQuery = Just Refl
    testEquality TimeZoneDayQuery TimeZoneDayQuery = Just Refl
    testEquality _ _ = Nothing

type DayFact = SomeOf DayQuery

readSpec_z :: String -> Maybe Int
readSpec_z = readTzOffset

readSpec_Z :: TimeLocale -> String -> Maybe TimeZone
readSpec_Z _ str | Just offset <- readTzOffset str = Just $ TimeZone offset False ""
readSpec_Z l str | Just zone <- getKnownTimeZone l str = Just zone
readSpec_Z _ "UTC" = Just utc
readSpec_Z _ [c] | Just zone <- getMilZone c = Just zone
readSpec_Z _ _ = Nothing

makeDayComponent :: TimeLocale -> Char -> String -> Maybe [DayFact]
makeDayComponent l c x =
    let
        ra :: forall a. Read a => Maybe a
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
                return [MkSomeOf CenturyDayQuery a]
            -- %f century (all but the last two digits of the year), 00 - 99
            'f' -> do
                a <- ra
                return [MkSomeOf CenturyDayQuery a]
            -- %Y: year
            'Y' -> do
                a <- ra
                return [MkSomeOf CenturyDayQuery $ a `div` 100, MkSomeOf YearOfCenturyDayQuery $ a `mod` 100]
            -- %G: year for Week Date format
            'G' -> do
                a <- ra
                return [MkSomeOf CenturyDayQuery $ a `div` 100, MkSomeOf YearOfCenturyDayQuery $ a `mod` 100]
            -- %y: last two digits of year, 00 - 99
            'y' -> do
                a <- ra
                return [MkSomeOf YearOfCenturyDayQuery a]
            -- %g: last two digits of year for Week Date format, 00 - 99
            'g' -> do
                a <- ra
                return [MkSomeOf YearOfCenturyDayQuery a]
            -- %B: month name, long form (fst from months locale), January - December
            'B' -> do
                a <- oneBasedListIndex $ fmap fst $ months l
                return [MkSomeOf MonthOfYearDayQuery a]
            -- %b: month name, short form (snd from months locale), Jan - Dec
            'b' -> do
                a <- oneBasedListIndex $ fmap snd $ months l
                return [MkSomeOf MonthOfYearDayQuery a]
            -- %m: month of year, leading 0 as needed, 01 - 12
            'm' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [MkSomeOf MonthOfYearDayQuery a]
            -- %d: day of month, leading 0 as needed, 01 - 31
            'd' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MkSomeOf DayOfMonthDayQuery a]
            -- %e: day of month, leading space as needed, 1 - 31
            'e' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MkSomeOf DayOfMonthDayQuery a]
            -- %V: week for Week Date format, 01 - 53
            'V' -> do
                raw <- ra
                a <- clipValid 1 53 raw
                return [MkSomeOf (WeekOfYearDayQuery ISOWeek) a]
            -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 00 - 53
            'U' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [MkSomeOf (WeekOfYearDayQuery SundayWeek) a]
            -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 00 - 53
            'W' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [MkSomeOf (WeekOfYearDayQuery MondayWeek) a]
            -- %u: day for Week Date format, 1 - 7
            'u' -> do
                raw <- ra
                a <- clipValid 1 7 raw
                return [MkSomeOf DayOfWeekDayQuery $ toEnum a]
            -- %a: day of week, short form (snd from wDays locale), Sun - Sat
            'a' -> do
                a <- zeroBasedListIndex $ fmap snd $ wDays l
                return [MkSomeOf DayOfWeekDayQuery $ toEnum a]
            -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
            'A' -> do
                a <- zeroBasedListIndex $ fmap fst $ wDays l
                return [MkSomeOf DayOfWeekDayQuery $ toEnum a]
            -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
            'w' -> do
                raw <- ra
                a <- clipValid 0 6 raw
                return [MkSomeOf DayOfWeekDayQuery $ toEnum a]
            -- %j: day of year for Ordinal Date format, 001 - 366
            'j' -> do
                raw <- ra
                a <- clipValid 1 366 raw
                return [MkSomeOf DayOfYearDayQuery a]
            -- %s: number of whole seconds since the Unix epoch.
            's' -> do
                raw <- ra
                return [MkSomeOf UTCTimeDayQuery $ posixSecondsToUTCTime $ fromInteger raw]
            'z' -> do
                a <- readSpec_z x
                return [MkSomeOf TimeZoneDayQuery $ TimeZone a False ""]
            'Z' -> do
                a <- readSpec_Z l x
                return [MkSomeOf TimeZoneDayQuery a]
            -- unrecognised, pass on to other parsers
            _ -> return []

makeDayComponents :: TimeLocale -> [(Char, String)] -> Maybe [DayFact]
makeDayComponents l pairs = do
    components <- for pairs $ \(c, x) -> makeDayComponent l c x
    return $ concat components

lastM :: [a] -> Maybe a
lastM [] = Nothing
lastM [a] = Just a
lastM (_ : aa) = lastM aa

safeLast :: a -> [a] -> a
safeLast x xs = fromMaybe x $ lastM xs

dcYear :: [DayFact] -> Integer
dcYear cs =
    let
        d = safeLast 70 [x | MkSomeOf YearOfCenturyDayQuery x <- cs]
        c =
            safeLast
                ( if d >= 69
                    then 19
                    else 20
                )
                [x | MkSomeOf CenturyDayQuery x <- cs]
    in
        100 * c + d

dcMatchLocalTime :: DayFact -> Maybe ([DayFact] -> LocalTime)
dcMatchLocalTime (MkSomeOf UTCTimeDayQuery t) = Just $ \cs ->
    let
        zone = safeLast utc [x | MkSomeOf TimeZoneDayQuery x <- cs]
    in
        utcToLocalTime zone t
dcMatchLocalTime _ = Nothing

dcMatchDay :: DayFact -> Maybe ([DayFact] -> Maybe Day)
dcMatchDay (MkSomeOf MonthOfYearDayQuery m) = Just $ \cs ->
    let
        y = dcYear cs
        d = safeLast 1 [x | MkSomeOf DayOfMonthDayQuery x <- cs]
    in
        fromGregorianValid y m d
dcMatchDay (MkSomeOf DayOfYearDayQuery d) = Just $ \cs ->
    let
        y = dcYear cs
    in
        fromOrdinalDateValid y d
dcMatchDay (MkSomeOf (WeekOfYearDayQuery wt) w) = Just $ \cs ->
    let
        y = dcYear cs
        d = fromEnum $ safeLast Thursday [x | MkSomeOf DayOfWeekDayQuery x <- cs]
    in
        case wt of
            ISOWeek -> fromWeekDateValid y w d
            SundayWeek -> fromSundayStartWeekValid y w (d `mod` 7)
            MondayWeek -> fromMondayStartWeekValid y w d
dcMatchDay comp | Just f <- dcMatchLocalTime comp = Just $ \cs -> return $ localDay $ f cs
dcMatchDay _ = Nothing

instance ParseTime Day where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        cs <- makeDayComponents l pairs
        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        let
            rest (comp : _) | Just f <- dcMatchDay comp = f cs
            rest (_ : xs) = rest xs
            rest [] = rest [MkSomeOf MonthOfYearDayQuery 1]
        rest cs

instance ParseTime DayOfWeek where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        cs <- makeDayComponents l pairs
        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        case lastM [x | MkSomeOf DayOfWeekDayQuery x <- cs] of
            Just x -> return x
            Nothing ->
                let
                    rest (comp : _) | Just f <- dcMatchDay comp = fmap dayOfWeek $ f cs
                    rest (_ : xs) = rest xs
                    rest [] = rest [MkSomeOf MonthOfYearDayQuery 1]
                in
                    rest cs

dayMonth :: Day -> Month
dayMonth (MonthDay m _) = m

instance ParseTime Month where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        cs <- makeDayComponents l pairs
        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        let
            y = dcYear cs
            rest (MkSomeOf MonthOfYearDayQuery m : _) = fromYearMonthValid y m
            rest (comp : _) | Just f <- dcMatchDay comp = fmap dayMonth $ f cs
            rest (_ : xs) = rest xs
            rest [] = fromYearMonthValid y 1
        rest cs

mfoldl :: Monad m => (a -> b -> m a) -> m a -> [b] -> m a
mfoldl f =
    let
        mf ma b = do
            a <- ma
            f a b
    in
        foldl mf

data AMPM = AM | PM

data TimeComponent
    = TCAMPM AMPM
    | TCHour Int
    | TCMinute Int
    | TCSecondInt Int
    | TCSecondFraction Pico
    | TCUTCTime UTCTime
    | TCTimeZone TimeZone

makeTimeComponent :: TimeLocale -> Char -> String -> Maybe [TimeComponent]
makeTimeComponent l c x =
    let
        ra :: Read a => Maybe a
        ra = readMaybe x
        getAmPm =
            let
                upx = map toUpper x
                (amStr, pmStr) = amPm l
            in
                if upx == amStr
                    then Just [TCAMPM AM]
                    else
                        if upx == pmStr
                            then Just [TCAMPM PM]
                            else Nothing
    in
        case c of
            'P' -> getAmPm
            'p' -> getAmPm
            'H' -> do
                raw <- ra
                a <- clipValid 0 23 raw
                return [TCHour a]
            'I' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [TCHour a]
            'k' -> do
                raw <- ra
                a <- clipValid 0 23 raw
                return [TCHour a]
            'l' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [TCHour a]
            'M' -> do
                raw <- ra
                a <- clipValid 0 59 raw
                return [TCMinute a]
            'S' -> do
                raw <- ra
                a <- clipValid 0 60 raw
                return [TCSecondInt $ fromInteger a]
            'q' -> do
                ps <- (readMaybe $ take 12 $ rpad 12 '0' x) <|> return 0
                return [TCSecondFraction $ mkPico 0 ps]
            'Q' ->
                if null x
                    then return []
                    else do
                        ps <- (readMaybe $ take 12 $ rpad 12 '0' x) <|> return 0
                        return [TCSecondFraction $ mkPico 0 ps]
            's' -> do
                raw <- ra
                return [TCUTCTime $ posixSecondsToUTCTime $ fromInteger raw]
            'z' -> do
                a <- readSpec_z x
                return [TCTimeZone $ TimeZone a False ""]
            'Z' -> do
                a <- readSpec_Z l x
                return [TCTimeZone a]
            _ -> return []

makeTimeComponents :: TimeLocale -> [(Char, String)] -> Maybe [TimeComponent]
makeTimeComponents l pairs = do
    components <- for pairs $ \(c, x) -> makeTimeComponent l c x
    return $ concat components

instance ParseTime TimeOfDay where
    substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l pairs = do
        cs <- makeTimeComponents l pairs
        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        case lastM [x | TCUTCTime x <- cs] of
            Just t ->
                let
                    zone = safeLast utc [x | TCTimeZone x <- cs]
                    sf = safeLast 0 [x | TCSecondFraction x <- cs]
                    TimeOfDay h m s = localTimeOfDay $ utcToLocalTime zone t
                in
                    return $ TimeOfDay h m $ s + sf
            Nothing ->
                let
                    h = safeLast 0 [x | TCHour x <- cs]
                    m = safeLast 0 [x | TCMinute x <- cs]
                    si = safeLast 0 [x | TCSecondInt x <- cs]
                    sf = safeLast 0 [x | TCSecondFraction x <- cs]
                    s :: Pico
                    s = fromIntegral si + sf
                    h' = case lastM [x | TCAMPM x <- cs] of
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
