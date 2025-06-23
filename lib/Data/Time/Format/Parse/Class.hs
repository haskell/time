{-# LANGUAGE Safe #-}

module Data.Time.Format.Parse.Class (
    -- * Parsing
    ParseNumericPadding (..),
    ParseTime (..),
    parseSpecifiers,
    timeSubstituteTimeSpecifier,
    timeParseTimeSpecifier,
    durationParseTimeSpecifier,
) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.Time.Format.Locale
import Text.ParserCombinators.ReadP

data ParseNumericPadding
    = NoPadding
    | SpacePadding
    | ZeroPadding

-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | @since 1.9.1
    substituteTimeSpecifier :: Proxy t -> TimeLocale -> Char -> Maybe String
    substituteTimeSpecifier _ _ _ = Nothing

    -- | Get the string corresponding to the given format specifier.
    --
    -- @since 1.9.1
    parseTimeSpecifier :: Proxy t -> TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String

    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    --
    -- @since 1.9.1
    buildTime ::
        -- | The time locale.
        TimeLocale ->
        -- | Pairs of format characters and the
        -- corresponding part of the input.
        [(Char, String)] ->
        Maybe t

-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.char'.
charCI :: Char -> ReadP Char
charCI c = satisfy (\x -> toUpper c == toUpper x)

-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.string'.
stringCI :: String -> ReadP String
stringCI this = do
    let
        scan [] _ = return this
        scan (x : xs) (y : ys)
            | toUpper x == toUpper y = do
                _ <- get
                scan xs ys
        scan _ _ = pfail
    s <- look
    scan this s

parseSpecifiers :: ParseTime t => Proxy t -> TimeLocale -> String -> ReadP [(Char, String)]
parseSpecifiers pt locale =
    let
        parse :: String -> ReadP [(Char, String)]
        parse [] = return []
        parse ('%' : cs) = parse1 cs
        parse (c : cs) | isSpace c = do
            _ <- satisfy isSpace
            case cs of
                (c' : _) | isSpace c' -> return ()
                _ -> skipSpaces
            parse cs
        parse (c : cs) = do
            _ <- charCI c
            parse cs
        parse1 :: String -> ReadP [(Char, String)]
        parse1 ('-' : cs) = parse2 (Just NoPadding) cs
        parse1 ('_' : cs) = parse2 (Just SpacePadding) cs
        parse1 ('0' : cs) = parse2 (Just ZeroPadding) cs
        parse1 cs = parse2 Nothing cs
        parse2 :: Maybe ParseNumericPadding -> String -> ReadP [(Char, String)]
        parse2 mpad ('E' : cs) = parse3 mpad True cs
        parse2 mpad cs = parse3 mpad False cs
        parse3 :: Maybe ParseNumericPadding -> Bool -> String -> ReadP [(Char, String)]
        parse3 _ _ ('%' : cs) = do
            _ <- char '%'
            parse cs
        parse3 _ _ (c : cs) | Just s <- substituteTimeSpecifier pt locale c = parse $ s ++ cs
        parse3 mpad _alt (c : cs) = do
            str <- parseTimeSpecifier pt locale mpad c
            specs <- parse cs
            return $ (c, str) : specs
        parse3 _ _ [] = return []
    in
        parse

data PaddingSide
    = PrePadding
    | PostPadding

data EmptyOption = AllowEmptyOption | ForbidEmptyOption

checkEmptyOption :: EmptyOption -> String -> ReadP ()
checkEmptyOption ForbidEmptyOption "" = mzero
checkEmptyOption _ _ = return ()

data MunchType = AmbiguousMunchType | MaximalMunchType

data Munch = InexactMunch MunchType | ExactMunch Int

munchDigits :: MunchType -> ReadP String
munchDigits AmbiguousMunchType = many $ satisfy isDigit
munchDigits MaximalMunchType = munch isDigit

checkAll :: (a -> Bool) -> [a] -> ReadP ()
checkAll f l = for_ l $ \c -> if f c then return () else mzero

parseAnyPaddedDigits :: Maybe PaddingSide -> Munch -> ReadP String
parseAnyPaddedDigits mpad (ExactMunch n) = do
    chars <- count n get
    case mpad of
        Nothing -> do
            checkAll isDigit chars
            return chars
        Just PrePadding -> do
            let
                digits = dropWhile isSpace chars
            checkAll isDigit digits
            return digits
        Just PostPadding -> do
            let
                (digits, spaces) = span isDigit chars
            checkAll isSpace spaces
            return digits
parseAnyPaddedDigits Nothing (InexactMunch munchtype) = munchDigits munchtype
parseAnyPaddedDigits (Just PrePadding) (InexactMunch munchtype) = do
    skipSpaces
    munchDigits munchtype
parseAnyPaddedDigits (Just PostPadding) (InexactMunch munchtype) = do
    r <- munchDigits munchtype
    skipSpaces
    return r

parsePaddedDigits :: Maybe PaddingSide -> Munch -> EmptyOption -> ReadP String
parsePaddedDigits mps mn eo = do
    digits <- parseAnyPaddedDigits mps mn
    checkEmptyOption eo digits
    return digits

parsePaddingDigits :: PaddingSide -> ParseNumericPadding -> EmptyOption -> MunchType -> Int -> ReadP String
parsePaddingDigits _ps NoPadding eo mt _n = parsePaddedDigits Nothing (InexactMunch mt) eo
parsePaddingDigits _ps ZeroPadding eo _mt n = parsePaddedDigits Nothing (ExactMunch n) eo
parsePaddingDigits ps SpacePadding eo mt _n = parsePaddedDigits (Just ps) (InexactMunch mt) eo

allowNegative :: ReadP String -> ReadP String
allowNegative p = do
    sign <- option "" $ fmap pure $ char '-'
    val <- p
    return $ sign ++ val

timeParseTimeSpecifier :: TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String
timeParseTimeSpecifier l mpad c =
    let
        parseDigits :: PaddingSide -> ParseNumericPadding -> EmptyOption -> MunchType -> Int -> ReadP String
        parseDigits ps pad = parsePaddingDigits ps $ fromMaybe pad mpad

        parseDigitsUsual :: ParseNumericPadding -> Int -> ReadP String
        parseDigitsUsual pad = parseDigits PrePadding pad ForbidEmptyOption AmbiguousMunchType

        oneOf = choice . map stringCI
        numericTZ = do
            s <- choice [char '+', char '-']
            h <- parseDigitsUsual ZeroPadding 2
            optional (char ':')
            m <- parseDigitsUsual ZeroPadding 2
            return (s : h ++ m)
    in
        case c of
            -- century
            'C' -> allowNegative $ parseDigitsUsual SpacePadding 2
            'f' -> allowNegative $ parseDigitsUsual SpacePadding 2
            -- year
            'Y' -> allowNegative $ parseDigitsUsual SpacePadding 4
            'G' -> allowNegative $ parseDigitsUsual SpacePadding 4
            -- year of century
            'y' -> parseDigitsUsual ZeroPadding 2
            'g' -> parseDigitsUsual ZeroPadding 2
            -- quarter of year
            'v' -> parseDigitsUsual ZeroPadding 1
            -- month of year
            'B' -> oneOf (map fst (months l))
            'b' -> oneOf (map snd (months l))
            'm' -> parseDigitsUsual ZeroPadding 2
            -- day of month
            'd' -> parseDigitsUsual ZeroPadding 2
            'e' -> parseDigitsUsual SpacePadding 2
            -- week of year
            'V' -> parseDigitsUsual ZeroPadding 2
            'U' -> parseDigitsUsual ZeroPadding 2
            'W' -> parseDigitsUsual ZeroPadding 2
            -- day of week
            'u' -> oneOf $ map (: []) ['1' .. '7']
            'a' -> oneOf (map snd (wDays l))
            'A' -> oneOf (map fst (wDays l))
            'w' -> oneOf $ map (: []) ['0' .. '6']
            -- day of year
            'j' -> parseDigitsUsual ZeroPadding 3
            -- dayhalf of day (i.e. AM or PM)
            'P' ->
                oneOf
                    ( let
                        (am, pm) = amPm l
                      in
                        [am, pm]
                    )
            'p' ->
                oneOf
                    ( let
                        (am, pm) = amPm l
                      in
                        [am, pm]
                    )
            -- hour of day (i.e. 24h)
            'H' -> parseDigitsUsual ZeroPadding 2
            'k' -> parseDigitsUsual SpacePadding 2
            -- hour of dayhalf (i.e. 12h)
            'I' -> parseDigitsUsual ZeroPadding 2
            'l' -> parseDigitsUsual SpacePadding 2
            -- minute of hour
            'M' -> parseDigitsUsual ZeroPadding 2
            -- second of minute
            'S' -> parseDigitsUsual ZeroPadding 2
            -- picosecond of second
            'q' -> parseDigits PostPadding NoPadding AllowEmptyOption MaximalMunchType 12
            'Q' -> (char '.' >> parseDigits PostPadding NoPadding AllowEmptyOption MaximalMunchType 12) <++ return ""
            -- time zone
            'z' -> numericTZ
            'Z' -> munch1 isAlpha <++ numericTZ
            -- seconds since epoch
            's' -> (char '-' >> fmap ('-' :) (munch1 isDigit)) <++ munch1 isDigit
            _ -> fail $ "Unknown format character: " ++ show c

timeSubstituteTimeSpecifier :: TimeLocale -> Char -> Maybe String
timeSubstituteTimeSpecifier l 'c' = Just $ dateTimeFmt l
timeSubstituteTimeSpecifier _ 'R' = Just "%H:%M"
timeSubstituteTimeSpecifier _ 'T' = Just "%H:%M:%S"
timeSubstituteTimeSpecifier l 'X' = Just $ timeFmt l
timeSubstituteTimeSpecifier l 'r' = Just $ time12Fmt l
timeSubstituteTimeSpecifier _ 'D' = Just "%m/%d/%y"
timeSubstituteTimeSpecifier _ 'F' = Just "%Y-%m-%d"
timeSubstituteTimeSpecifier l 'x' = Just $ dateFmt l
timeSubstituteTimeSpecifier _ 'h' = Just "%b"
timeSubstituteTimeSpecifier _ _ = Nothing

durationParseTimeSpecifier :: TimeLocale -> Maybe ParseNumericPadding -> Char -> ReadP String
durationParseTimeSpecifier _ mpad c =
    let
        parsePaddedSignedDigits :: Int -> ReadP String
        parsePaddedSignedDigits n = allowNegative $ do
            parsePaddingDigits PrePadding (fromMaybe NoPadding mpad) ForbidEmptyOption MaximalMunchType n

        parseSignedDecimal :: ReadP String
        parseSignedDecimal = allowNegative $ do
            digits <- munch1 isDigit
            decimaldigits <-
                option "" $ do
                    _ <- char '.'
                    dd <- munch isDigit
                    return $ '.' : dd
            return $ digits ++ decimaldigits
    in
        case c of
            'y' -> parsePaddedSignedDigits 1
            'b' -> parsePaddedSignedDigits 1
            'B' -> parsePaddedSignedDigits 2
            'w' -> parsePaddedSignedDigits 1
            'd' -> parsePaddedSignedDigits 1
            'D' -> parsePaddedSignedDigits 1
            'h' -> parsePaddedSignedDigits 1
            'H' -> parsePaddedSignedDigits 2
            'm' -> parsePaddedSignedDigits 1
            'M' -> parsePaddedSignedDigits 2
            's' -> parseSignedDecimal
            'S' -> parseSignedDecimal
            _ -> fail $ "Unknown format character: " ++ show c
