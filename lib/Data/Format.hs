{-# LANGUAGE Safe #-}

module Data.Format (
    Productish (..),
    Summish (..),
    parseReader,
    Format (..),
    formatShow,
    formatParseM,
    isoMap,
    mapMFormat,
    filterFormat,
    clipFormat,
    enumMap,
    literalFormat,
    specialCaseShowFormat,
    specialCaseFormat,
    optionalFormat,
    casesFormat,
    optionalSignFormat,
    mandatorySignFormat,
    SignOption (..),
    integerFormat,
    decimalFormat,
) where

import Control.Monad.Fail
import Data.Char
import Data.Void
import Text.ParserCombinators.ReadP
import Prelude hiding (fail)

class IsoVariant f where
    isoMap :: (a -> b) -> (b -> a) -> f a -> f b

enumMap :: (IsoVariant f, Enum a) => f Int -> f a
enumMap = isoMap toEnum fromEnum

infixr 3 <**>, **>, <**

class IsoVariant f => Productish f where
    pUnit :: f ()
    (<**>) :: f a -> f b -> f (a, b)
    (**>) :: f () -> f a -> f a
    fu **> fa = isoMap (\((), a) -> a) (\a -> ((), a)) $ fu <**> fa
    (<**) :: f a -> f () -> f a
    fa <** fu = isoMap (\(a, ()) -> a) (\a -> (a, ())) $ fa <**> fu

infixr 2 <++>

class IsoVariant f => Summish f where
    pVoid :: f Void
    (<++>) :: f a -> f b -> f (Either a b)

parseReader :: MonadFail m => ReadP t -> String -> m t
parseReader readp s =
    case [t | (t, "") <- readP_to_S readp s] of
        [t] -> return t
        [] -> fail $ "no parse of " ++ show s
        _ -> fail $ "multiple parses of " ++ show s

-- | A text format for a type
data Format t = MkFormat
    { formatShowM :: t -> Maybe String
    -- ^ Show a value in the format, if representable
    , formatReadP :: ReadP t
    -- ^ Read a value in the format
    }

-- | Show a value in the format, or error if unrepresentable
formatShow :: Format t -> t -> String
formatShow fmt t =
    case formatShowM fmt t of
        Just str -> str
        Nothing -> error "formatShow: bad value"

-- | Parse a value in the format
formatParseM :: MonadFail m => Format t -> String -> m t
formatParseM format = parseReader $ formatReadP format

instance IsoVariant Format where
    isoMap ab ba (MkFormat sa ra) = MkFormat (\b -> sa $ ba b) (fmap ab ra)

mapMFormat :: (a -> Maybe b) -> (b -> Maybe a) -> Format a -> Format b
mapMFormat amb bma (MkFormat sa ra) =
    MkFormat (\b -> bma b >>= sa) $ do
        a <- ra
        case amb a of
            Just b -> return b
            Nothing -> pfail

filterFormat :: (a -> Bool) -> Format a -> Format a
filterFormat test =
    mapMFormat
        ( \a ->
            if test a
                then Just a
                else Nothing
        )
        ( \a ->
            if test a
                then Just a
                else Nothing
        )

-- | Limits are inclusive
clipFormat :: Ord a => (a, a) -> Format a -> Format a
clipFormat (lo, hi) = filterFormat (\a -> a >= lo && a <= hi)

instance Productish Format where
    pUnit = MkFormat{formatShowM = \_ -> Just "", formatReadP = return ()}
    (<**>) (MkFormat sa ra) (MkFormat sb rb) =
        let
            sab (a, b) = do
                astr <- sa a
                bstr <- sb b
                return $ astr ++ bstr
            rab = do
                a <- ra
                b <- rb
                return (a, b)
        in
            MkFormat sab rab
    (MkFormat sa ra) **> (MkFormat sb rb) =
        let
            s b = do
                astr <- sa ()
                bstr <- sb b
                return $ astr ++ bstr
            r = do
                ra
                rb
        in
            MkFormat s r
    (MkFormat sa ra) <** (MkFormat sb rb) =
        let
            s a = do
                astr <- sa a
                bstr <- sb ()
                return $ astr ++ bstr
            r = do
                a <- ra
                rb
                return a
        in
            MkFormat s r

instance Summish Format where
    pVoid = MkFormat absurd pfail
    (MkFormat sa ra) <++> (MkFormat sb rb) =
        let
            sab (Left a) = sa a
            sab (Right b) = sb b
            rab = (fmap Left ra) +++ (fmap Right rb)
        in
            MkFormat sab rab

literalFormat :: String -> Format ()
literalFormat s = MkFormat{formatShowM = \_ -> Just s, formatReadP = string s >> return ()}

specialCaseShowFormat :: Eq a => (a, String) -> Format a -> Format a
specialCaseShowFormat (val, str) (MkFormat s r) =
    let
        s' t
            | t == val = Just str
        s' t = s t
    in
        MkFormat s' r

specialCaseFormat :: Eq a => (a, String) -> Format a -> Format a
specialCaseFormat (val, str) (MkFormat s r) =
    let
        s' t
            | t == val = Just str
        s' t = s t
        r' = r <++ (string str >> return val)
    in
        MkFormat s' r'

optionalFormat :: Eq a => a -> Format a -> Format a
optionalFormat val = specialCaseFormat (val, "")

casesFormat :: Eq a => [(a, String)] -> Format a
casesFormat pairs =
    let
        s t = lookup t pairs
        r [] = pfail
        r ((v, str) : pp) = (string str >> return v) <++ r pp
    in
        MkFormat s $ r pairs

optionalSignFormat :: (Eq t, Num t) => Format t
optionalSignFormat = casesFormat [(1, ""), (1, "+"), (0, ""), (-1, "-")]

mandatorySignFormat :: (Eq t, Num t) => Format t
mandatorySignFormat = casesFormat [(1, "+"), (0, "+"), (-1, "-")]

data SignOption
    = NoSign
    | NegSign
    | PosNegSign

readSign :: Num t => SignOption -> ReadP (t -> t)
readSign NoSign = return id
readSign NegSign = option id $ char '-' >> return negate
readSign PosNegSign = (char '+' >> return id) +++ (char '-' >> return negate)

readNumber :: (Num t, Read t) => SignOption -> Maybe Int -> Bool -> ReadP t
readNumber signOpt mdigitcount allowDecimal = do
    sign <- readSign signOpt
    digits <-
        case mdigitcount of
            Just digitcount -> count digitcount $ satisfy isDigit
            Nothing -> munch1 isDigit
    moredigits <-
        case allowDecimal of
            False -> return ""
            True ->
                option "" $ do
                    _ <- char '.' +++ char ','
                    dd <- munch1 isDigit
                    return $ '.' : dd
    return $ sign $ read $ digits ++ moredigits

zeroPad :: Maybe Int -> String -> String
zeroPad Nothing s = s
zeroPad (Just i) s = replicate (i - length s) '0' ++ s

trimTrailing :: String -> String
trimTrailing
    = (\s -> if s == "." then "" else s)
    . reverse
    . dropWhile (== '0')
    . reverse

showNumber :: Show t => SignOption -> Maybe Int -> t -> Maybe String
showNumber signOpt mdigitcount t =
    let
        showIt str =
            let
                (intPart, decPart) = break ((==) '.') str
            in
                (zeroPad mdigitcount intPart) ++ trimTrailing decPart
    in
        case show t of
            ('-' : str) ->
                case signOpt of
                    NoSign -> Nothing
                    _ -> Just $ '-' : showIt str
            str ->
                Just $
                    case signOpt of
                        PosNegSign -> '+' : showIt str
                        _ -> showIt str

integerFormat :: (Show t, Read t, Num t) => SignOption -> Maybe Int -> Format t
integerFormat signOpt mdigitcount = MkFormat (showNumber signOpt mdigitcount) (readNumber signOpt mdigitcount False)

decimalFormat :: (Show t, Read t, Num t) => SignOption -> Maybe Int -> Format t
decimalFormat signOpt mdigitcount = MkFormat (showNumber signOpt mdigitcount) (readNumber signOpt mdigitcount True)
