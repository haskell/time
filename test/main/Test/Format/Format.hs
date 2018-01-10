module Test.Format.Format(testFormat) where

import Data.Time
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil


-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- f not supported
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

-- as found in "man strftime" on a glibc system. '#' is different, though
modifiers :: [Char]
modifiers = "_-0^"

widths :: [String]
widths = ["","1","2","9","12"]

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"] ++ (fmap (\char -> '%':[char]) chars)
 ++ (concat $ fmap (\char -> concat $ fmap (\width -> fmap (\modifier -> "%" ++ [modifier] ++ width ++ [char]) modifiers) widths) chars)

somestrings :: [String]
somestrings = ["", " ", "-", "\n"]

compareExpected :: (Eq t,Show t,ParseTime t) => String -> String -> String -> proxy t -> TestTree
compareExpected testname fmt str proxy = testCase testname $ do
    let
        found :: ParseTime t => proxy t -> Maybe t
        found _ = parseTimeM False defaultTimeLocale fmt str
    assertEqual "" Nothing $ found proxy

checkParse :: String -> String -> [TestTree]
checkParse fmt str = [
    compareExpected "Day" fmt str (Proxy :: Proxy Day),
    compareExpected "TimeOfDay" fmt str (Proxy :: Proxy TimeOfDay),
    compareExpected "LocalTime" fmt str (Proxy :: Proxy LocalTime),
    compareExpected "TimeZone" fmt str (Proxy :: Proxy TimeZone),
    compareExpected "UTCTime" fmt str (Proxy :: Proxy UTCTime)
    ]

testCheckParse :: TestTree
testCheckParse = testGroup "checkParse" $ tgroup formats $ \fmt -> tgroup somestrings $ \str -> checkParse fmt str

days :: [Day]
days = [(fromGregorian 2018 1 5) .. (fromGregorian 2018 1 26)]

testDayOfWeek :: TestTree
testDayOfWeek  = testGroup "DayOfWeek" $ tgroup "uwaA" $ \fmt -> tgroup days $ \day -> let
    dayFormat = formatTime defaultTimeLocale ['%',fmt] day
    dowFormat = formatTime defaultTimeLocale ['%',fmt] $ dayOfWeek day
    in assertEqual "" dayFormat dowFormat

testFormat :: TestTree
testFormat = testGroup "testFormat" $ [
    testCheckParse,
    testDayOfWeek
    ]
