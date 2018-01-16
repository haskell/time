module Data.Time.Calendar.CalendarDiffDays
    (
        -- * Calendar Duration
        module Data.Time.Calendar.CalendarDiffDays
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup hiding (option)
#endif
import Data.Char
import Text.ParserCombinators.ReadP hiding (string)

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    } deriving Eq

#if MIN_VERSION_base(4,9,0)
-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

-- | Additive
instance Monoid CalendarDiffDays where
    mempty = CalendarDiffDays 0 0
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend (CalendarDiffDays m1 d1) (CalendarDiffDays m2 d2) = CalendarDiffDays (m1 + m2) (d1 + d2)
#endif

-- | Show in ISO 8601 \"PyyYmmMddD\" format. (Zero items will be omitted, except zero time will be \"P0D\".)
instance Show CalendarDiffDays where
    show dur@(CalendarDiffDays m d) = let
        (y,my) = quotRem m 12
        ys = if y == 0 then "" else show y ++ "Y"
        ms = if my == 0 then "" else show my ++ "M"
        ds = if d == 0 then "" else show d ++ "D"
        in if dur == mempty then "P0D" else "P" ++ ys ++ ms ++ ds

-- | Read in ISO 8601 \"PyyYmmMwwWddD\" format. (Items may be omitted.)
instance Read CalendarDiffDays where
    readsPrec _ = readParen False $ readP_to_S $ skipSpaces >> do
        let
            ch :: Char -> ReadP ()
            ch c = char c >> return ()

            readInteger :: ReadP Integer
            readInteger = do
                neg <- option False $ ch '-' >> return True
                digits <- many1 (satisfy isDigit)
                return $ (if neg then negate else id) $ read digits

            readItem :: Char -> ReadP Integer
            readItem c = option 0 $ do
                i <- readInteger
                ch c
                return i
        ch 'P'
        y <- readItem 'Y'
        m <- readItem 'M'
        w <- readItem 'W'
        d <- readItem 'D'
        return $ CalendarDiffDays (y * 12 + m) (w * 7 + d)

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)
