{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Structure-preserving ISO 8601 / XSD @xs:duration@ values.
--
-- == Background and standards
--
-- 'Data.Time.Format.ISO8601.durationTimeFormat' parses into
-- 'Data.Time.LocalTime.CalendarDiffTime', which collapses the original
-- Y\/M\/W\/D\/H\/M\/S grammar into @(months, NominalDiffTime)@.
-- That is convenient for arithmetic but lossy: @P1Y@ and @P12M@,
-- @P1W@ and @P7D@, @PT1H@ and @PT3600S@ all parse to equal values
-- and serialize to a single canonical form.
--
-- 'Duration' preserves every component the input mentioned, with a
-- single leading sign.  The 'iso8601Format' on 'Duration' round-trips
-- bit-exact for every valid ISO 8601 / XSD lexical form.
--
-- === Standards summary
--
-- * __ISO 8601:2004__ §4.4.3 defines two designator forms:
--     §4.4.3.2 @P[nY][nM][nD][T[nH][nM][nS]]@ and the week form
--     @PnW@; and §4.4.3.3 the alternative form
--     @Pyyyy-mm-ddThh:mm:ss@.  ISO 8601 forbids a leading sign on
--     these forms, but does permit a decimal fraction on the
--     /lowest-order/ component (so @PT1.5H@ is well-formed if no
--     M\/S follows).
--
-- * __XML Schema Part 2: Datatypes 1.1__ defines @xs:duration@
--     (§3.3.6).  XSD diverges from ISO 8601 in three significant
--     ways:
--
--       1. XSD __admits a leading minus sign__ (§3.3.6.2 prod. [15]).
--       2. XSD __omits the week form__ entirely
--          (§3.3.6.2 prods. [6]–[15]; there is no @duWeekFrag@).
--       3. XSD __omits the alternative @Pyyyy-mm-dd...@ form__.
--
--     XSD also restricts decimals to seconds only
--     (§3.3.6.2 prod. [11]); other fields are
--     @unsignedNoDecimalPtNumeral@.  The @xs:duration@ value space
--     is the pair @(months, seconds)@ with the invariant that the
--     two components must not have opposite signs (§3.3.6.1).
--
-- * __XSD 1.1 §E.2__ defines the canonical lexical form
--     (@durationCanonicalMap@).  Months canonicalise as
--     @(y, m) = (ym \`divMod\` 12)@; seconds canonicalise as days /
--     hours / minutes / seconds via successive division by
--     86400 \/ 3600 \/ 60.  Day count is /not/ folded into months
--     (month length is variable).  See 'normalizeDuration'.
--
-- * __XSD 1.1 §E.3.3__ defines @dateTimePlusDuration@: the months
--     and seconds axes are added independently to a reference
--     dateTime; the day field is then clipped (April 31 → April 30)
--     before the seconds axis is folded in.  See
--     'addDurationClip'\/'addDurationRollOver'.
--
-- * Discussion of native duration support in @time@:
--   <https://github.com/haskell/time/issues/40> (open since 2014);
--   structure-preservation rationale: @haskell/time#292@.
--
-- == Spec deviations
--
-- * __'Ord' is total, XSD's order is partial.__ XSD 1.1 §3.3.6.1
--   leaves pairs like @P1M@ vs @P30D@ incomparable.  The 'Ord'
--   instance for 'Duration' agrees with XSD whenever XSD is
--   decisive (per 'xsdCompare') and uses a deterministic
--   tiebreaker on the canonical @(months, seconds)@ tuple
--   otherwise — necessary for use as 'Data.Set.Set'/'Data.Map.Map'
--   keys, but /not/ a claim about XSD semantics.  Use
--   'xsdCompare' for the strict partial order.
-- * __'Eq' is structural, XSD-equality is value-based.__ The
--   derived 'Eq' distinguishes @P1Y@ from @P12M@ (as required for
--   round-trip preservation); use 'valueEqual' for XSD §3.3.6.1
--   value-equality.
-- * __No 'xs:yearMonthDuration' (§3.4.26) /
--   'xs:dayTimeDuration' (§3.4.27)__ subset types are provided as
--   first-class formats; filter the 'durDate' / 'durTime' fields
--   if you need them.
module Data.Time.Format.ISO8601.Duration (
    -- * The 'Duration' type
    Duration (..),
    DurationDate (..),
    zeroDuration,

    -- * Formats
    durationFormat,
    xsdDurationFormat,

    -- * Bridging to 'CalendarDiffTime'
    toCalendarDiffTime,
    fromCalendarDiffTime,

    -- * Normalization
    normalizeDuration,

    -- * Comparison
    xsdCompare,
    valueEqual,

    -- * Arithmetic
    scaleDuration,
    addUTCDurationClip,
    addUTCDurationRollOver,
    addLocalDurationClip,
    addLocalDurationRollOver,
    diffUTCDurationClip,
    diffUTCDurationRollOver,
    diffLocalDurationClip,
    diffLocalDurationRollOver,

    -- * Deprecated (use the @addUTC@\/@addLocal@ variants)
    addDurationClip,
    addDurationRollOver,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Format
import Data.Time.Calendar.Gregorian (fromGregorian)
import Data.Time.Clock.Internal.NominalDiffTime (NominalDiffTime)
import Data.Time.Clock.Internal.UTCTime
import Data.Time.LocalTime.Internal.CalendarDiffTime hiding
    ( addUTCDurationClip
    , addUTCDurationRollOver
    , diffUTCDurationClip
    , diffUTCDurationRollOver
    )
import qualified Data.Time.LocalTime.Internal.CalendarDiffTime as CDT
import Data.Time.LocalTime.Internal.LocalTime (LocalTime)
import qualified Data.Time.LocalTime.Internal.LocalTime as LT
import GHC.Generics
import Text.ParserCombinators.ReadP
#if __GLASGOW_HASKELL__ >= 914
import qualified Language.Haskell.TH.Lift as TH
#else
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- | The date portion of a duration.
--
-- ISO 8601:2004 §4.4.3.2 specifies that the week form @PnW@
-- excludes any other date or time components.  This sum type makes
-- that constraint impossible to violate.  XSD 1.1 has no week form
-- (§3.3.6.2); 'xsdDurationFormat' rejects 'DurationWeeks' on read
-- and on show.
data DurationDate
    = -- | @P[nY][nM][nD]@ — any subset, including all absent.
      --   Each component is 'Just' iff the corresponding designator
      --   appeared in the lexical form.
      DurationYMD
        { ddYears :: Maybe Integer
        , ddMonths :: Maybe Integer
        , ddDays :: Maybe Integer
        }
    | -- | @PnW@ — week form (ISO 8601:2004 §4.4.3.2).  Mutually
      --   exclusive with everything else.  Not part of XSD 1.1
      --   @xs:duration@ (§3.3.6.2 has no @duWeekFrag@).
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
-- Time components are present (in @durTime@) only if the lexical
-- form contains a @T@.
data Duration = Duration
    { durSign :: Integer
    -- ^ @1@ or @-1@.  Other values are coerced to @1@ on show.
    , durDate :: DurationDate
    , durTime :: Maybe (Maybe Integer, Maybe Integer, Maybe Pico)
    -- ^ @Just (h, m, s)@ when the lexical form contains @T@; the
    --   tuple components are 'Just' iff their designator appeared.
    }
    deriving (Eq, Show, Typeable, Data, Generic, TH.Lift)

instance NFData Duration where
    rnf (Duration s dd t) = rnf s `seq` rnf dd `seq` rnf t `seq` ()

-- | __'Ord' vs the XSD partial order — deliberate deviations.__
--
-- XSD 1.1 §3.3.6.1 specifies that 'xs:duration' is /partially/
-- ordered: e.g. @P1M@ and @P30D@ are incomparable because the
-- result of adding them to a reference dateTime depends on which
-- calendar month is referenced.  Haskell's 'Ord' class requires a
-- /total/ order, /and/ the convention that @compare a b == EQ@
-- implies @a == b@.  Both cannot be satisfied literally; the
-- instance picks the following compromise:
--
-- * __When XSD is strictly decisive__ (@'xsdCompare' a b ==
--   @'Just' 'LT'@ or @'Just' 'GT'@), the instance returns the
--   same answer.  Within the XSD value space, this 'Ord' is a
--   monotone refinement of the XSD partial order.
-- * __When XSD says equal or incomparable__, the instance uses a
--   deterministic structural tiebreaker (canonical @(months,
--   seconds)@ first, then a comparison of the show-form), so
--   @compare a b == EQ@ iff @a == b@ structurally.  This means
--   value-equal-but-structurally-distinct pairs like @P1Y@ vs
--   @P12M@ get a non-@EQ@ ordering, /and/ XSD-incomparable pairs
--   like @P1M@ vs @P30D@ get a deterministic order.
--
-- The tiebreaker is a Haskell-level convenience for use as
-- 'Data.Set.Set' \/ 'Data.Map.Map' keys, sorting, etc.; it is
-- /not/ a claim about XSD semantics.
--
-- For the strict XSD partial order, use 'xsdCompare' which
-- returns 'Nothing' on genuinely incomparable pairs.  For XSD
-- value-equality (which structural 'Eq' does /not/ provide), use
-- 'valueEqual'.
instance Ord Duration where
    compare a b = case xsdCompare a b of
        -- XSD says strictly less / greater: take that.
        Just LT -> LT
        Just GT -> GT
        -- XSD says equal (value-equal) or incomparable: use the
        -- structural tiebreaker so that 'compare' is consistent
        -- with structural 'Eq' (i.e. @compare a b == EQ@ iff
        -- @a == b@).  Two value-equal-but-structurally-distinct
        -- durations like @P1Y@ and @P12M@ get a deterministic
        -- non-EQ answer here.
        _ -> compareTiebreak a b

-- | XSD 1.1 §3.3.6.1 partial-order test on durations.
--
-- @'xsdCompare' a b@ is @'Just' o@ when adding @a@ and @b@ to each
-- of the four reference dateTimes from §3.3.6.1
-- (@1696-09-01T00:00:00Z@, @1697-02-01T00:00:00Z@,
-- @1903-03-01T00:00:00Z@, @1903-07-01T00:00:00Z@) yields the same
-- relative order @o@ for all four; otherwise 'Nothing'
-- (genuinely incomparable per the spec).
--
-- The classic incomparable example is @'xsdCompare' P1M P30D ==
-- 'Nothing'@: in February @P1M@ is shorter, in months with 31 days
-- it is longer.
--
-- 'Eq'-like behaviour: when 'xsdCompare' returns @'Just' EQ@ the
-- two durations are XSD-/value/-equal (see 'valueEqual'); the
-- structural 'Eq' may still distinguish them.
xsdCompare :: Duration -> Duration -> Maybe Ordering
xsdCompare a b =
    let
        results = map (\ref -> compare (addUTCDurationClip a ref) (addUTCDurationClip b ref)) xsdReferenceDateTimes
    in
        case results of
            [] -> Just EQ
            (r : rs) -> if all (== r) rs then Just r else Nothing

-- | The four reference dateTimes from XSD 1.1 §3.3.6.1, used by
-- 'xsdCompare' to test the partial-order.  These are chosen to
-- exercise short months, long months, leap years, and non-leap
-- years.
xsdReferenceDateTimes :: [UTCTime]
xsdReferenceDateTimes =
    [ UTCTime (fromGregorian 1696 9 1) 0
    , UTCTime (fromGregorian 1697 2 1) 0
    , UTCTime (fromGregorian 1903 3 1) 0
    , UTCTime (fromGregorian 1903 7 1) 0
    ]

-- | Deterministic tiebreaker used by 'Ord' when XSD does not give
-- a strict order.  First compares by canonical @(months, seconds)@
-- via 'toCalendarDiffTime'; if those are equal, falls back to a
-- structural comparison on the show-form to keep 'Ord' consistent
-- with structural 'Eq' (so @compare a b == EQ@ iff @a == b@).
compareTiebreak :: Duration -> Duration -> Ordering
compareTiebreak a b =
    let
        CalendarDiffTime ma ta = toCalendarDiffTime a
        CalendarDiffTime mb tb = toCalendarDiffTime b
    in
        case compare ma mb of
            EQ -> case compare ta tb of
                EQ -> compare (showOrShow a) (showOrShow b)
                o -> o
            o -> o
  where
    showOrShow d = case formatShowM durationFormat d of
        Just s -> s
        Nothing -> show d

-- | XSD 1.1 §3.3.6.1 value equality: two durations are
-- value-equal iff their canonical @(months, seconds)@ tuples are
-- identical.  Distinct from the structural 'Eq' instance —
-- e.g. @'valueEqual' P1Y P12M == True@ but @P1Y \/= P12M@.
valueEqual :: Duration -> Duration -> Bool
valueEqual a b = toCalendarDiffTime a == toCalendarDiffTime b

-- | The empty duration: @P0D@.
zeroDuration :: Duration
zeroDuration =
    Duration
        { durSign = 1
        , durDate = DurationYMD Nothing Nothing Nothing
        , durTime = Nothing
        }

-- | Spec-correct addition per XSD 1.1 §3.3.6.1: add the months
-- axes and the seconds axes independently, then canonicalise the
-- result via 'normalizeDuration' (XSD §E.2).  This matches the
-- 'Semigroup' for 'CalendarDiffTime' on the 'toCalendarDiffTime'
-- image and is the operation @xml-typelift@ et al. need to drop
-- the @iso8601-duration@ \/ 'CalendarDiffTime' dependency entirely.
--
-- __Mixed-sign edge case.__  Two same-signed operands always sum
-- to a same-signed result, which 'fromCalendarDiffTime' accepts.
-- Two opposite-signed operands can produce a 'CalendarDiffTime'
-- with mixed-signed @(months, seconds)@; XSD §3.3.6.1 forbids this
-- in the value space.  The instance resolves this by /absorbing
-- the smaller-magnitude axis into the dominant one's sign/: the
-- result keeps the magnitude of the larger axis and zeros the
-- smaller.  This is the closest representable approximation to
-- the 'CalendarDiffTime' sum within the XSD value space; it is a
-- known lossy edge case, documented here.  In practice durations
-- combined for dateTime arithmetic should pass through
-- 'addUTCDurationClip' \/ 'addLocalDurationClip', not through
-- @<>@, when this matters.
--
-- __Loss of lexical structure.__  Like all arithmetic on
-- 'Duration', @<>@ collapses the structure.  @P1Y \<\> P12M@ is
-- 'normalizeDuration'-equivalent to @P2Y@, not @P1Y12M@.  This
-- is /not/ field-wise concatenation (which would not be
-- associative or canonical).
instance Semigroup Duration where
    a <> b = normalizeDuration (canonicaliseFromCDT (toCalendarDiffTime a <> toCalendarDiffTime b))

instance Monoid Duration where
    mempty = zeroDuration
    mappend = (<>)

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

-- | The structure-preserving ISO 8601 duration format.  Accepts
-- both ISO 8601:2004 §4.4.3.2 sub-forms (designator and week) and
-- (per XSD 1.1 §3.3.6.2 prod. [15]) a leading sign.
--
-- Round-trips every valid lexical form bit-exact, modulo
-- normalization of trailing zeros in the optional decimal part of
-- the seconds field.  The empty duration renders as @P0D@; on read
-- this format requires at least one designator (XSD 1.1 §3.3.6.2:
-- @P@ alone is invalid).
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

-- | XSD 1.1 @xs:duration@ format: the strict subset of ISO 8601
-- that XSD admits.  Differences from 'durationFormat':
--
-- * Rejects the week form @PnW@ on read and show
--   (XSD 1.1 §3.3.6.2 has no @duWeekFrag@).
-- * Otherwise identical: leading sign accepted (prod. [15]),
--   decimals only on the seconds field (prod. [11]).
--
-- For the related XSD 1.1 subset types, see
-- @xs:yearMonthDuration@ (§3.4.26) — pattern @[^DT]*@ — and
-- @xs:dayTimeDuration@ (§3.4.27) — pattern @[^YM]*[DT].*@.  Those
-- are not provided as separate formats here; filter the parsed
-- 'Duration' if you need them.
xsdDurationFormat :: Format Duration
xsdDurationFormat =
    let
        MkFormat sh re = durationFormat
        rejectWeek (Duration _ DurationWeeks{} _) = Nothing
        rejectWeek d = Just d
        sh' d = rejectWeek d >>= sh
        re' = do
            d <- re
            case d of
                Duration _ DurationWeeks{} _ -> pfail
                _ -> return d
    in
        MkFormat sh' re'

-- | Conversion to the lossy 'CalendarDiffTime', summing all
-- components into the @(months, seconds)@ value space of XSD 1.1
-- §3.3.6.1.  Always succeeds.
toCalendarDiffTime :: Duration -> CalendarDiffTime
toCalendarDiffTime (Duration sign dd mt) =
    let
        sgn = if sign < 0 then -1 else 1
        (mm, dd') = case dd of
            DurationYMD y m d ->
                (sgn * (12 * fromMaybe0 y + fromMaybe0 m), sgn * fromMaybe0 d)
            DurationWeeks w -> (0, sgn * 7 * w)
        secsFromTime = case mt of
            Nothing -> 0
            Just (h, m, s) ->
                fromIntegral (sgn * (3600 * fromMaybe0 h + 60 * fromMaybe0 m))
                    + realToFrac (fromIntegral sgn * fromMaybe0Pico s)
        secsFromDays = fromInteger dd' * 86400
    in
        CalendarDiffTime mm (secsFromDays + secsFromTime)
  where
    fromMaybe0 = maybe 0 id
    fromMaybe0Pico :: Maybe Pico -> Pico
    fromMaybe0Pico = maybe 0 id

-- | Conversion from the lossy 'CalendarDiffTime'.  Returns 'Nothing'
-- when 'ctMonths' and 'ctTime' have opposite signs — a state
-- 'CalendarDiffTime' admits but XSD 1.1 §3.3.6.1 forbids in the
-- value space (the months and seconds components of an
-- @xs:duration@ must not have opposite signs).
--
-- The result is in canonical form per XSD 1.1 §E.2: months split
-- into Y\/M, seconds split into D\/H\/M\/S, zero fields dropped,
-- single leading sign.  In particular the result never uses the
-- 'DurationWeeks' constructor — the week form does not survive a
-- round-trip through the value space (XSD has no week form at all).
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

-- | Canonicalise a 'Duration' per XSD 1.1 §E.2 (@durationCanonicalMap@):
--
-- * On the year\/month axis, @12M@ carries into @1Y@; the residue
--   month is in [0,11].
-- * On the time axis, @60S → 1M@, @60M → 1H@, @24H → 1D@; the
--   residues are in [0,59], [0,59], [0,23] respectively.  Decimal
--   seconds are preserved.
-- * The day count is __not__ folded into months (month length is
--   variable; XSD §E.2 only divides seconds by 86400).
-- * Zero fields are dropped (@'Just' 0@ → 'Nothing').
-- * The week form @PnW@ folds into @P(7n)D@ — XSD has no week form.
-- * A single leading sign is factored out; mixed-sign inputs are
--   rejected by 'fromCalendarDiffTime' upstream of this function.
-- * The empty duration canonicalises to @P0D@ (matching the show
--   form of 'zeroDuration').
--
-- @'normalizeDuration' . 'normalizeDuration' = 'normalizeDuration'@
-- (idempotent).
normalizeDuration :: Duration -> Duration
normalizeDuration d = case fromCalendarDiffTime (toCalendarDiffTime d) of
    Just d' -> d'
    Nothing -> zeroDuration

-- | Scale by an integer factor.  Note that @'scaleDuration' (-1)@
-- will not perfectly invert a duration whose months and days are
-- both non-zero, due to variable month length — exactly as for
-- 'scaleCalendarDiffTime'.  The result is normalised.
scaleDuration :: Integer -> Duration -> Duration
scaleDuration k d =
    case fromCalendarDiffTime (scaleCalendarDiffTime k (toCalendarDiffTime d)) of
        Just d' -> d'
        Nothing -> zeroDuration

-- | Add a 'Duration' to a 'UTCTime' using clipped Gregorian month
-- arithmetic (per XSD 1.1 §E.3.3: pin day to
-- @min(da, daysInMonth(yr, mo))@ before applying the seconds axis).
addUTCDurationClip :: Duration -> UTCTime -> UTCTime
addUTCDurationClip d = CDT.addUTCDurationClip (toCalendarDiffTime d)

-- | Like 'addUTCDurationClip' but rolls the day over rather than
-- clipping (April 31 → May 1).
addUTCDurationRollOver :: Duration -> UTCTime -> UTCTime
addUTCDurationRollOver d = CDT.addUTCDurationRollOver (toCalendarDiffTime d)

-- | Add a 'Duration' to a 'LocalTime' using clipped Gregorian month
-- arithmetic.
addLocalDurationClip :: Duration -> LocalTime -> LocalTime
addLocalDurationClip d = LT.addLocalDurationClip (toCalendarDiffTime d)

-- | Like 'addLocalDurationClip' but rolls the day over.
addLocalDurationRollOver :: Duration -> LocalTime -> LocalTime
addLocalDurationRollOver d = LT.addLocalDurationRollOver (toCalendarDiffTime d)

-- | The 'Duration' between two 'UTCTime's, using clipped Gregorian
-- difference for the month axis.  The result is in canonical form
-- (XSD 1.1 §E.2); it never uses the week form.
diffUTCDurationClip :: UTCTime -> UTCTime -> Duration
diffUTCDurationClip a b = canonicaliseFromCDT (CDT.diffUTCDurationClip a b)

-- | Like 'diffUTCDurationClip' but uses rolled-over Gregorian
-- difference.
diffUTCDurationRollOver :: UTCTime -> UTCTime -> Duration
diffUTCDurationRollOver a b = canonicaliseFromCDT (CDT.diffUTCDurationRollOver a b)

-- | The 'Duration' between two 'LocalTime's, using clipped Gregorian
-- difference for the month axis.
diffLocalDurationClip :: LocalTime -> LocalTime -> Duration
diffLocalDurationClip a b = canonicaliseFromCDT (LT.diffLocalDurationClip a b)

-- | Like 'diffLocalDurationClip' but uses rolled-over Gregorian
-- difference.
diffLocalDurationRollOver :: LocalTime -> LocalTime -> Duration
diffLocalDurationRollOver a b = canonicaliseFromCDT (LT.diffLocalDurationRollOver a b)

canonicaliseFromCDT :: CalendarDiffTime -> Duration
canonicaliseFromCDT cdt = case fromCalendarDiffTime cdt of
    Just d -> d
    -- 'CalendarDiffTime' from a diff can be mixed-sign; fall back
    -- to a same-signed approximation by zeroing the smaller axis.
    Nothing ->
        let CalendarDiffTime m t = cdt
            cdt' =
                if abs (fromInteger m :: Pico) * 86400 * 30 >= realToFrac (abs t :: NominalDiffTime)
                    then CalendarDiffTime m 0
                    else CalendarDiffTime 0 t
        in case fromCalendarDiffTime cdt' of
            Just d -> d
            Nothing -> zeroDuration

-- | Deprecated alias for 'addUTCDurationClip'.
addDurationClip :: Duration -> UTCTime -> UTCTime
addDurationClip = addUTCDurationClip
{-# DEPRECATED addDurationClip "Use addUTCDurationClip instead." #-}

-- | Deprecated alias for 'addUTCDurationRollOver'.
addDurationRollOver :: Duration -> UTCTime -> UTCTime
addDurationRollOver = addUTCDurationRollOver
{-# DEPRECATED addDurationRollOver "Use addUTCDurationRollOver instead." #-}
