{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Structure-preserving ISO 8601 / XSD @xs:duration@ values.
--
-- 'Data.Time.Format.ISO8601.durationTimeFormat' parses into
-- 'Data.Time.LocalTime.CalendarDiffTime', collapsing the original
-- Y\/M\/W\/D\/H\/M\/S grammar into @(months, NominalDiffTime)@.
-- That is convenient for arithmetic but lossy: @P1Y@ and @P12M@,
-- @P1W@ and @P7D@, @PT1H@ and @PT3600S@ all parse to equal values
-- and serialize to a single canonical form.  XML Schema
-- @xs:duration@ requires lexical-space round-trip; many other
-- consumers (JSON Schema, ICS, OData, REST APIs) expect it as well.
--
-- 'Duration' keeps every component the input mentioned, including
-- the ISO 8601 week form (which is mutually exclusive with the
-- Y\/M\/D\/T components per ISO 8601:2004 sec. 4.4.3.2) and a single
-- leading sign.  'iso8601Format' on 'Duration' is bit-exact for
-- every valid lexical form, modulo trailing zeros after a decimal
-- point in the seconds field (per the format-class normalization).
module Data.Time.Format.ISO8601.Duration (
    Duration (..),
    DurationDate (..),
    zeroDuration,
    durationFormat,
    toCalendarDiffTime,
    fromCalendarDiffTime,
    addDurationClip,
    addDurationRollOver,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Format
import Data.Time.Clock.Internal.UTCTime
import Data.Time.LocalTime.Internal.CalendarDiffTime
import GHC.Generics
import Text.ParserCombinators.ReadP
#if __GLASGOW_HASKELL__ >= 914
import qualified Language.Haskell.TH.Lift as TH
#else
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- | The date portion of a duration.
--
-- ISO 8601:2004 sec. 4.4.3.2 specifies that the week form @PnW@
-- excludes any other date or time components.  This sum type makes
-- that constraint impossible to violate.
data DurationDate
    = -- | @P[nY][nM][nD]@ — any subset, including all absent.
      --   Each component is 'Just' iff the corresponding designator
      --   appeared in the lexical form.
      DurationYMD
        { ddYears :: Maybe Integer
        , ddMonths :: Maybe Integer
        , ddDays :: Maybe Integer
        }
    | -- | @PnW@ — week form, mutually exclusive with everything else.
      DurationWeeks
        { ddWeeks :: Integer
        }
    deriving (Eq, Show, Typeable, Data, Generic, TH.Lift)

instance NFData DurationDate where
    rnf (DurationYMD y m d) = rnf y `seq` rnf m `seq` rnf d `seq` ()
    rnf (DurationWeeks w) = rnf w `seq` ()

-- | A structure-preserving ISO 8601 / XSD @xs:duration@ value.
--
-- ISO 8601 permits a single leading sign covering the whole value;
-- per-field signs are not permitted.  'durSign' carries that sign.
-- Time components are present (in 'durHours', 'durMinutes',
-- 'durSeconds') only if the lexical form contains a @T@.
data Duration = Duration
    { durSign :: Integer
    -- ^ @1@ or @-1@.  Other values render as @1@.
    , durDate :: DurationDate
    , durTime :: Maybe (Maybe Integer, Maybe Integer, Maybe Pico)
    -- ^ @Just (h, m, s)@ when the lexical form contains @T@; the
    --   tuple components are 'Just' iff their designator appeared.
    }
    deriving (Eq, Show, Typeable, Data, Generic, TH.Lift)

instance NFData Duration where
    rnf (Duration s dd t) = rnf s `seq` rnf dd `seq` rnf t `seq` ()

-- | The empty duration: @P0D@.
zeroDuration :: Duration
zeroDuration =
    Duration
        { durSign = 1
        , durDate = DurationYMD Nothing Nothing Nothing
        , durTime = Nothing
        }

-- | A 'Format' for an optional designator-suffixed component.  Unlike
-- the local 'optionalFormat' helper, this preserves the distinction
-- between \"absent\" and \"zero\".
maybeDesignator ::
    (Show t, Read t, Num t, Eq t) =>
    -- | sign handling for the number itself (always 'NoSign' for ISO 8601)
    SignOption ->
    -- | designator letter
    Char ->
    -- | whether this is the seconds field (decimal allowed)
    Bool ->
    Format (Maybe t)
maybeDesignator signOpt c allowDecimal =
    let
        numFmt =
            if allowDecimal
                then decimalFormat signOpt Nothing
                else integerFormat signOpt Nothing
        present = isoMap Just (\m -> case m of Just x -> x; Nothing -> 0) $ numFmt <** literalFormat [c]
        showFn Nothing = Just ""
        showFn jx = formatShowM present jx
        readFn = formatReadP present <++ return Nothing
    in
        MkFormat showFn readFn

ymdFormat :: Format DurationDate
ymdFormat =
    let
        toDD (y, (m, d)) = DurationYMD y m d
        fromDD (DurationYMD y m d) = Just (y, (m, d))
        fromDD _ = Nothing
    in
        mapMFormat (Just . toDD) fromDD $
            maybeDesignator NoSign 'Y' False
                <**> maybeDesignator NoSign 'M' False
                <**> maybeDesignator NoSign 'D' False

weekFormat :: Format DurationDate
weekFormat =
    let
        toDD w = DurationWeeks w
        fromDD (DurationWeeks w) = Just w
        fromDD _ = Nothing
        wFmt = integerFormat NoSign Nothing <** literalFormat "W"
    in
        mapMFormat (Just . toDD) fromDD wFmt

dateFormat :: Format DurationDate
dateFormat =
    let
        showFn dd = case dd of
            DurationWeeks _ -> formatShowM weekFormat dd
            DurationYMD{} -> formatShowM ymdFormat dd
        readFn = formatReadP weekFormat <++ formatReadP ymdFormat
    in
        MkFormat showFn readFn

timeFormat :: Format (Maybe Integer, Maybe Integer, Maybe Pico)
timeFormat =
    let
        toT (h, (m, s)) = (h, m, s)
        fromT (h, m, s) = (h, (m, s))
    in
        isoMap toT fromT $
            maybeDesignator NoSign 'H' False
                <**> maybeDesignator NoSign 'M' False
                <**> maybeDesignator NoSign 'S' True

optionalTimeFormat :: Format (Maybe (Maybe Integer, Maybe Integer, Maybe Pico))
optionalTimeFormat =
    let
        showFn Nothing = Just ""
        showFn (Just t) = do
            inner <- formatShowM timeFormat t
            return $ 'T' : inner
        readFn =
            (do
                _ <- string "T"
                t <- formatReadP timeFormat
                return $ Just t
            )
                <++ return Nothing
    in
        MkFormat showFn readFn

signFormat :: Format Integer
signFormat =
    let
        showFn s
            | s < 0 = Just "-"
            | otherwise = Just ""
        readFn = (char '-' >> return (-1)) <++ return 1
    in
        MkFormat showFn readFn

-- | The structure-preserving ISO 8601 duration format.
--
-- Round-trips every valid ISO 8601 lexical form bit-exact, modulo
-- normalization of trailing zeros in the optional decimal part of
-- the seconds field.  The empty duration renders as @P0D@; on read
-- this format requires at least one designator.
durationFormat :: Format Duration
durationFormat =
    let
        toD (s, (dd, t)) = Duration s dd t
        fromD (Duration s dd t) = Just (s, (dd, t))
        body = (signFormat <** literalFormat "P") <**> dateFormat <**> optionalTimeFormat
        full = mapMFormat (Just . toD) fromD body

        showFn d
            | d == zeroDuration = Just "P0D"
            | otherwise = formatShowM full d
        readFn = do
            d <- formatReadP full
            case d of
                Duration _ (DurationYMD Nothing Nothing Nothing) Nothing -> pfail
                Duration _ _ (Just (Nothing, Nothing, Nothing)) -> pfail
                _ -> return d
    in
        MkFormat showFn readFn

-- | Conversion to the lossy 'CalendarDiffTime', summing all
-- components into @(months, time)@.  Always succeeds.
toCalendarDiffTime :: Duration -> CalendarDiffTime
toCalendarDiffTime (Duration sign dd mt) =
    let
        (mm, dd') = case dd of
            DurationYMD y m d ->
                (sign * (12 * fromMaybe0 y + fromMaybe0 m), sign * fromMaybe0 d)
            DurationWeeks w -> (0, sign * 7 * w)
        secsFromTime = case mt of
            Nothing -> 0
            Just (h, m, s) ->
                fromIntegral (sign * (3600 * fromMaybe0 h + 60 * fromMaybe0 m))
                    + realToFrac (fromIntegral sign * fromMaybe0Pico s)
        secsFromDays = fromInteger dd' * 86400
    in
        CalendarDiffTime mm (secsFromDays + secsFromTime)
  where
    fromMaybe0 = maybe 0 id
    fromMaybe0Pico :: Maybe Pico -> Pico
    fromMaybe0Pico = maybe 0 id

-- | Conversion from the lossy 'CalendarDiffTime'.  Returns 'Nothing'
-- when the value cannot be expressed as a sign-and-magnitude ISO
-- 8601 duration: that is, when 'ctMonths' and 'ctTime' have opposite
-- signs.  Lossless representations are not produced for the year /
-- week designators, since 'CalendarDiffTime' does not carry that
-- information; the result uses month and day designators only.
fromCalendarDiffTime :: CalendarDiffTime -> Maybe Duration
fromCalendarDiffTime (CalendarDiffTime mm t)
    | mm < 0 && t > 0 = Nothing
    | mm > 0 && t < 0 = Nothing
    | otherwise =
        let
            sign = if mm < 0 || t < 0 then -1 else 1
            absMonths = abs mm
            absT = abs t
            totalPico = realToFrac absT :: Pico
            -- split into whole days and remaining seconds
            wholeSecs = floor (toRational totalPico) :: Integer
            (days, secsRem) = wholeSecs `quotRem` 86400
            fracPart = totalPico - fromInteger wholeSecs
            (h, hRem) = secsRem `quotRem` 3600
            (mn, sInt) = hRem `quotRem` 60
            secs = fromInteger sInt + fracPart
            timeAll =
                if h == 0 && mn == 0 && secs == 0
                    then Nothing
                    else
                        Just
                            ( if h == 0 then Nothing else Just h
                            , if mn == 0 then Nothing else Just mn
                            , if secs == 0 then Nothing else Just secs
                            )
            datePart =
                DurationYMD
                    (if absMonths >= 12 then Just (absMonths `div` 12) else Nothing)
                    (case absMonths `mod` 12 of 0 -> Nothing; m -> Just m)
                    (if days == 0 then Nothing else Just days)
        in
            Just $ Duration sign datePart timeAll

-- | Add a 'Duration' to a 'UTCTime' using clipped Gregorian month
-- arithmetic.
addDurationClip :: Duration -> UTCTime -> UTCTime
addDurationClip d = addUTCDurationClip (toCalendarDiffTime d)

-- | Add a 'Duration' to a 'UTCTime' using rolled-over Gregorian
-- month arithmetic.
addDurationRollOver :: Duration -> UTCTime -> UTCTime
addDurationRollOver d = addUTCDurationRollOver (toCalendarDiffTime d)
