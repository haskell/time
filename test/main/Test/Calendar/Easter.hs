module Test.Calendar.Easter (
    testEaster,
) where

import Data.Foldable
import Data.Time.Calendar
import Data.Time.Calendar.Easter
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read

easterDatesFile :: FilePath
easterDatesFile = "test/main/Test/Calendar/EasterData/easter500.txt"

allEasterDatesFile :: FilePath
allEasterDatesFile = "test/main/Test/Calendar/EasterData/easter-reference-1900-2099.csv"

parseField :: Read a => String -> IO a
parseField field =
    case readMaybe field of
        Just value -> return value
        Nothing -> fail $ "invalid integer field: " ++ show field

parseEasterDates :: [String] -> IO [Day]
parseEasterDates [] = return []
parseEasterDates (monthText : dayText : yearText : rest) = do
    month <- parseField monthText
    day <- parseField dayText
    year <- parseField yearText
    easterDay <-
        maybe (fail $ "invalid date: " ++ unwords [yearText, monthText, dayText]) return $
            fromGregorianValid year month day
    (easterDay :) <$> parseEasterDates rest
parseEasterDates fields = fail $ "easter500.txt: expected month/day/year triples, leftover fields: " ++ show fields

getEasterDates :: IO [Day]
getEasterDates = do
    contents <- readFile easterDatesFile
    parseEasterDates $ words contents

splitCommas :: String -> [String]
splitCommas [] = [""]
splitCommas (',' : rest) = "" : splitCommas rest
splitCommas (char : rest) =
    case splitCommas rest of
        [] -> [[char]]
        field : fields -> (char : field) : fields

testGregorianEaster :: TestTree
testGregorianEaster =
    testCase "Gregorian Easter" $ do
        easterDates <- getEasterDates
        for_ easterDates $ \expectedDate@(YearMonthDay y _ _) -> do
            let
                foundDate = gregorianEaster y
            assertEqual "Gregorian Easter" expectedDate foundDate

getAllEasterDates :: IO [(Day, Day, Day, Day)]
getAllEasterDates = do
    contents <- readFile allEasterDatesFile
    parseAllEasterDates $ lines contents

parseAllEasterDates :: [String] -> IO [(Day, Day, Day, Day)]
parseAllEasterDates [] = fail $ allEasterDatesFile ++ ": empty file"
parseAllEasterDates (header : rows) = do
    let
        expectedHeader =
            [ "year"
            , "golden_number"
            , "gregorian_pfm"
            , "gregorian_easter"
            , "orthodox_pfm_gregorian"
            , "orthodox_easter_gregorian"
            ]
    if splitCommas header == expectedHeader
        then traverse parseAllEasterDateLine $ zip [2 :: Int ..] rows
        else fail $ allEasterDatesFile ++ ": unexpected header: " ++ show header

parseAllEasterDateLine :: (Int, String) -> IO (Day, Day, Day, Day)
parseAllEasterDateLine (lineNumber, line) =
    case splitCommas line of
        [yearText, _, gregorianMoonText, gregorianEasterText, orthodoxMoonText, orthodoxEasterText] -> do
            year <- parseField yearText
            gregorianMoon <- parseDayField lineNumber "gregorian_pfm" gregorianMoonText
            gregorianEasterDay <- parseDayField lineNumber "gregorian_easter" gregorianEasterText
            orthodoxMoon <- parseDayField lineNumber "orthodox_pfm_gregorian" orthodoxMoonText
            orthodoxEasterDay <- parseDayField lineNumber "orthodox_easter_gregorian" orthodoxEasterText
            let
                YearMonthDay gregorianMoonYear _ _ = gregorianMoon
            if gregorianMoonYear == year
                then return (gregorianMoon, gregorianEasterDay, orthodoxMoon, orthodoxEasterDay)
                else fail $ allEasterDatesFile ++ ":" ++ show lineNumber ++ ": row year does not match Gregorian Paschal moon"
        fields -> fail $ allEasterDatesFile ++ ":" ++ show lineNumber ++ ": expected 6 CSV fields, found " ++ show (length fields)

parseDayField :: Int -> String -> String -> IO Day
parseDayField lineNumber fieldName field =
    case splitDate field of
        [yearText, monthText, dayText] -> do
            year <- parseField yearText
            month <- parseField monthText
            day <- parseField dayText
            maybe
                (fail $ allEasterDatesFile ++ ":" ++ show lineNumber ++ ": invalid " ++ fieldName ++ ": " ++ show field)
                return
                $ fromGregorianValid year month day
        _ -> fail $ allEasterDatesFile ++ ":" ++ show lineNumber ++ ": invalid " ++ fieldName ++ ": " ++ show field

splitDate :: String -> [String]
splitDate [] = [""]
splitDate ('-' : rest) = "" : splitDate rest
splitDate (char : rest) =
    case splitDate rest of
        [] -> [[char]]
        field : fields -> (char : field) : fields

testAllEasters :: TestTree
testAllEasters =
    testCase "all Easters" $ do
        allEasterDates <- getAllEasterDates
        for_ allEasterDates $ \(expectedGM@(YearMonthDay y _ _), expectedGE, expectedOM, expectedOE) -> do
            assertEqual "Gregorian Paschal moon" expectedGM $ gregorianPaschalMoon y
            assertEqual "Gregorian Easter" expectedGE $ gregorianEaster y
            assertEqual "Orthodox Paschal moon" expectedOM $ orthodoxPaschalMoon y
            assertEqual "Orthodox Easter" expectedOE $ orthodoxEaster y

testEaster :: TestTree
testEaster = testGroup "testEaster" [testGregorianEaster, testAllEasters]
