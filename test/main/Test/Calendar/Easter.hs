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

testGregorianEaster :: TestTree
testGregorianEaster =
    testCase "Gregorian Easter" $ do
        easterDates :: [Day] <- getEasterDates
        for_ easterDates $ \expectedDate@(YearMonthDay y _ _) -> do
            let
                foundDate = gregorianEaster y
            assertEqual "Gregorian Easter" expectedDate foundDate

testEaster :: TestTree
testEaster = testGroup "testEaster" [testGregorianEaster]
