module Test.TestValid where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.Julian
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)
import Test.QuickCheck.Property


validResult :: (Eq c,Show c,Eq t,Show t) =>
    Bool -> (t -> c) -> (c -> t) -> (c -> Maybe t) -> c -> Result
validResult valid toComponents fromComponents fromComponentsValid c = let
    mt = fromComponentsValid c
    t' = fromComponents c
    c' = toComponents t'
    in if valid then
        case mt of
            Nothing -> rejected
            Just t -> if t' /= t
                then failed {reason = "'fromValid' gives " ++ show t ++ ", but 'from' gives " ++ show t'}
                else if c' /= c
                then failed {reason = "found valid, but converts " ++ show c ++ " -> " ++ show t' ++ " -> " ++ show c'}
                else succeeded
        else case mt of
            Nothing -> if c' /= c
                then succeeded
                else failed {reason = show c ++ " found invalid, but converts with " ++ show t'}
            Just _ -> rejected

validTest :: (Arbitrary c,Eq c,Show c,Eq t,Show t) =>
    String -> (t -> c) -> (c -> t) -> (c -> Maybe t) -> TestTree
validTest name toComponents fromComponents fromComponentsValid = testGroup name
    [
    testProperty "valid" $ property $ validResult True toComponents fromComponents fromComponentsValid,
    testProperty "invalid" $ property $ validResult False toComponents fromComponents fromComponentsValid
    ]

toSundayStartWeek :: Day -> (Integer,Int,Int)
toSundayStartWeek day = let
    (y,_) = toOrdinalDate day
    (m,d) = sundayStartWeek day
    in (y,m,d)

toMondayStartWeek :: Day -> (Integer,Int,Int)
toMondayStartWeek day = let
    (y,_) = toOrdinalDate day
    (m,d) = mondayStartWeek day
    in (y,m,d)

testValid :: TestTree
testValid = testGroup "testValid"
    [
    validTest "Gregorian" toGregorian (\(y,m,d) -> fromGregorian y m d) (\(y,m,d) -> fromGregorianValid y m d),
    validTest "OrdinalDate" toOrdinalDate (\(y,d) -> fromOrdinalDate y d) (\(y,d) -> fromOrdinalDateValid y d),
    validTest "WeekDate" toWeekDate (\(y,m,d) -> fromWeekDate y m d) (\(y,m,d) -> fromWeekDateValid y m d),
    validTest "SundayStartWeek" toSundayStartWeek (\(y,m,d) -> fromSundayStartWeek y m d) (\(y,m,d) -> fromSundayStartWeekValid y m d),
    validTest "MondayStartWeek" toMondayStartWeek (\(y,m,d) -> fromMondayStartWeek y m d) (\(y,m,d) -> fromMondayStartWeekValid y m d),
    validTest "Julian" toJulian (\(y,m,d) -> fromJulian y m d) (\(y,m,d) -> fromJulianValid y m d)
    ]
