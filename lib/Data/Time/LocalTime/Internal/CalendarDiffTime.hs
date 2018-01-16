module Data.Time.LocalTime.Internal.CalendarDiffTime
    (
        -- * Calendar Duration
        module Data.Time.LocalTime.Internal.CalendarDiffTime
    ) where
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup hiding (option)
#endif
import Data.Fixed
import Data.Char
import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Text.ParserCombinators.ReadP hiding (string)

data CalendarDiffTime = CalendarDiffTime
    { ctMonths :: Integer
    , ctTime :: NominalDiffTime
    } deriving (Eq)
#if MIN_VERSION_base(4,9,0)
-- | Additive
instance Semigroup CalendarDiffTime where
    CalendarDiffTime m1 d1 <> CalendarDiffTime m2 d2 = CalendarDiffTime (m1 + m2) (d1 + d2)
#endif
-- | Additive
instance Monoid CalendarDiffTime where
    mempty = CalendarDiffTime 0 0
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend (CalendarDiffTime m1 d1) (CalendarDiffTime m2 d2) = CalendarDiffTime (m1 + m2) (d1 + d2)
#endif
-- | Show in ISO 8601 \"PyyYmmMddDThhHmmMssS\" format. (Zero items will be omitted, and a zero time part with be entirely omitted, except zero time will be \"P0D\".)
instance Show CalendarDiffTime where
    show (CalendarDiffTime months t) = let
        part :: (Eq t, Num t, Show t) => t -> String -> String
        part x s =
            if x == 0
                then ""
                else show x ++ s
        part' :: Pico -> String -> String
        part' x s =
            if x == 0
                then ""
                else showFixed True x ++ s
        (y, ym) = quotRem months 12
        (d, dh, hm, ms) =
            if t >= 0
                then let
                         (d', TimeOfDay dh' hm' ms') = timeToDaysAndTimeOfDay t
                         in (d', dh', hm', ms')
                else let
                         (d', TimeOfDay dh' hm' ms') = timeToDaysAndTimeOfDay $ negate t
                         in (negate d', negate dh', negate hm', negate ms')
        dpart = mconcat [part y "Y", part ym "M", part d "D"]
        tpart = mconcat [part dh "H", part hm "M", part' ms "S"]
        in "P" ++
           case (dpart, tpart) of
               ("", "") -> "0D"
               (_, "") -> dpart
               (_, _) -> dpart ++ "T" ++ tpart

-- | Read in ISO 8601 \"PyyYmmMwwWddDThhHmmMssS\" format. (Items may be omitted, and the \"T\" time section may be entirely omitted.)
instance Read CalendarDiffTime where
    readsPrec _ =
        readParen False $
        readP_to_S $
        skipSpaces >> do
            let
                ch :: Char -> ReadP ()
                ch c = char c >> return ()
                readPositiveInteger ::  ReadP Integer
                readPositiveInteger = do
                    digits <- many1 (satisfy isDigit)
                    return $ read digits
                readPositivePico :: ReadP Pico
                readPositivePico = do
                    digits <- many1 (satisfy isDigit)
                    moredigits <- option "" $ do
                        ch '.'
                        dd <- many1 (satisfy isDigit)
                        return $ '.':dd
                    return $ read $ digits ++ moredigits
                readItem :: Num t => ReadP t -> Char -> ReadP t
                readItem readPositive c =
                    option 0 $ do
                        neg <- option False $ ch '-' >> return True
                        x <- readPositive
                        ch c
                        return $ if neg then negate x else x
            ch 'P'
            y <- readItem readPositiveInteger 'Y'
            m <- readItem readPositiveInteger 'M'
            w <- readItem readPositiveInteger 'W'
            d <- readItem readPositiveInteger 'D'
            let
                months = y * 12 + m
                days = w * 7 + d
            seconds <-
                option 0 $ do
                    ch 'T'
                    dh <- readItem readPositiveInteger 'H'
                    hm <- readItem readPositiveInteger 'M'
                    ms <- readItem readPositivePico 'S'
                    return $ ms + 60 * (fromInteger hm + 60 * fromInteger dh)
            return $ CalendarDiffTime months $ (fromInteger days * nominalDay) + realToFrac seconds

calendarTimeDays :: CalendarDiffDays -> CalendarDiffTime
calendarTimeDays (CalendarDiffDays m d) = CalendarDiffTime m $ fromInteger d * nominalDay

-- | Scale by a factor. Note that @scaleCalendarDiffTime (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffTime :: Integer -> CalendarDiffTime -> CalendarDiffTime
scaleCalendarDiffTime k (CalendarDiffTime m d) = CalendarDiffTime (k * m) (fromInteger k * d)
