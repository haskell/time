{-# OPTIONS -XForeignFunctionInterface -Wall -Werror #-}

module Test.TestFormat where

import Data.Time
import Data.Time.Clock.POSIX

import Data.Char
import Data.Functor

import System.Locale
import Foreign
import Foreign.C
import Control.Exception;

import Test.TestUtil

--

{-
	size_t format_time (
	char *s, size_t maxsize,
	const char *format,
	int isdst,int gmtoff,time_t t);
-}

foreign import ccall unsafe "TestFormatStuff.h format_time" format_time :: CString -> CSize -> CString -> CInt -> CInt -> CString -> CTime -> IO CSize

withBuffer :: Int -> (CString -> IO CSize) -> IO String
withBuffer n f = withArray (replicate n 0) (\buffer -> do
			len <- f buffer
			peekCStringLen (buffer,fromIntegral len)
		)

unixFormatTime :: String -> TimeZone -> UTCTime -> IO String
unixFormatTime fmt zone time = withCString fmt (\pfmt -> withCString (timeZoneName zone) (\pzonename ->
		withBuffer 100 (\buffer -> format_time buffer 100 pfmt
				(if timeZoneSummerOnly zone then 1 else 0)
				(fromIntegral (timeZoneMinutes zone * 60))
				pzonename
				(fromInteger (truncate (utcTimeToPOSIXSeconds time)))
			)
		))

locale :: TimeLocale
locale = defaultTimeLocale {dateTimeFmt = "%a %b %e %H:%M:%S %Y"}

zones :: [TimeZone]
zones = [utc,TimeZone 87 True "Fenwickian Daylight Time"]

baseTime0 :: UTCTime
baseTime0 = localTimeToUTC utc (LocalTime (fromGregorian 1970 01 01) midnight)

baseTime1 :: UTCTime
baseTime1 = localTimeToUTC utc (LocalTime (fromGregorian 2000 01 01) midnight)

getDay :: Integer -> UTCTime
getDay day = addUTCTime ((fromInteger day) * posixDayLength) baseTime1

getYearP1 :: Integer -> UTCTime
getYearP1 year = localTimeToUTC utc (LocalTime (fromGregorian year 01 01) midnight)

getYearP2 :: Integer -> UTCTime
getYearP2 year = localTimeToUTC utc (LocalTime (fromGregorian year 02 04) midnight)

getYearP3 :: Integer -> UTCTime
getYearP3 year = localTimeToUTC utc (LocalTime (fromGregorian year 03 04) midnight)

getYearP4 :: Integer -> UTCTime
getYearP4 year = localTimeToUTC utc (LocalTime (fromGregorian year 12 31) midnight)

times :: [UTCTime]
times = [baseTime0] ++ (fmap getDay [0..23]) ++ (fmap getDay [0..100]) ++
	(fmap getYearP1 [1980..2000]) ++ (fmap getYearP2 [1980..2000]) ++ (fmap getYearP3 [1980..2000]) ++ (fmap getYearP4 [1980..2000])

compareFormat :: (String -> String) -> String -> TimeZone -> UTCTime -> IO Bool
compareFormat modUnix fmt zone time
  = do let ctime = utcToZonedTime zone time
           haskellText = formatTime locale fmt ctime
       unixText <- fmap modUnix (unixFormatTime fmt zone time)
       if haskellText == unixText
         then return True -- ""
         else return False
           {- unwords
                [ "Mismatch with", fmt, "for"
                , show ctime ++ ": UNIX=\"" ++ unixText ++ "\", TimeLib=\"" ++ haskellText ++ "\"."] -}

-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

-- as found in "man strftime" on a glibc system. '#' is different, though
modifiers :: [Char]
modifiers = "_-0^"

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"] ++ (fmap (\char -> '%':char:[]) chars)
 ++ (concat (fmap (\char -> fmap (\modifier -> '%':modifier:char:[]) modifiers) chars))

hashformats :: [String]
hashformats =  (fmap (\char -> '%':'#':char:[]) chars)

somestrings :: [String]
somestrings = ["", " ", "-", "\n"]

getBottom :: a -> IO (Maybe Control.Exception.SomeException);
getBottom a = Control.Exception.catch (seq a (return Nothing)) (return . Just);    

safeString :: String -> IO String
safeString s = do
 msx <- getBottom s
 case msx of
  Just sx -> return (show sx)
  Nothing -> case s of
   (c:cc) -> do
    mcx <- getBottom c
    case mcx of
     Just cx -> return (show cx)
     Nothing -> do
      ss <- safeString cc
      return (c:ss)
   [] -> return ""

compareExpected :: (Eq t,Show t,ParseTime t) => String -> String -> String -> Maybe t -> IO Bool
compareExpected _ fmt str expected
   = do let found = parseTime defaultTimeLocale fmt str
        mex <- getBottom found
        case mex of
          Just _ -> return False
                  {- return $ unwords
                       [ "Exception with", fmt
                       , "for", ts
                       , show str ++ ": expected"
                       , show expected ++ ", caught", show ex] -}

          Nothing -> 
            if found == expected
              then return True -- return ""
              else return False
                {- do sf <- safeString (show found)
                      return $ unwords 
                        [ "Mismatch with", fmt
                        , "for", ts
                        , show str ++ ": expected"
                        , show expected ++ ", found", sf] -}

class (ParseTime t) => TestParse t where
    expectedParse :: String -> String -> Maybe t
    expectedParse "%Z" str | all isSpace str = Just (buildTime defaultTimeLocale [])
    expectedParse "%_Z" str | all isSpace str = Just (buildTime defaultTimeLocale [])
    expectedParse "%-Z" str | all isSpace str = Just (buildTime defaultTimeLocale [])
    expectedParse "%0Z" str | all isSpace str = Just (buildTime defaultTimeLocale [])
    expectedParse _ _ = Nothing

instance TestParse Day
instance TestParse TimeOfDay
instance TestParse LocalTime
instance TestParse TimeZone
instance TestParse ZonedTime
instance TestParse UTCTime

checkParse :: String -> String -> IO [Bool]
checkParse fmt str
  = sequence [ compareExpected "Day" fmt str (expectedParse fmt str :: Maybe Day)
             , compareExpected "TimeOfDay" fmt str (expectedParse fmt str :: Maybe TimeOfDay)
             , compareExpected "LocalTime" fmt str (expectedParse fmt str :: Maybe LocalTime)
             , compareExpected "TimeZone" fmt str (expectedParse fmt str :: Maybe TimeZone)
             , compareExpected "UTCTime" fmt str (expectedParse fmt str :: Maybe UTCTime) ]

testFormat :: Test
testFormat
  = impure $ IO_SimpleTest "testFormat"
      $ do a <- concat <$>  mapM (\fmt -> concat <$> mapM (checkParse fmt) somestrings) formats
           let a' = if all (== True) a
                      then Pass
                      else Fail $ "testFormat: checkParse failed"
         
	   b <- mapM (\fmt -> mapM (\time -> mapM (\zone -> compareFormat id fmt zone time) zones) times) formats
           let b' = if all (== True) $ concat $ concat b
                      then Pass
                      else Fail $ "testFormat: compareFormat failed on variable formats"

	   c <- mapM (\fmt -> mapM (\time -> mapM (\zone -> compareFormat (fmap toLower) fmt zone time) zones) times) hashformats
           let c' = if all (== True) $ concat $ concat c
                      then Pass
                      else Fail $ "testFormat: compareFormat failed on variable hashFormats"
          
           let fs = concatFailures [a', b', c']
           return $ if null fs  then Pass  else Fail $ fs 

concatFailures :: [Result] -> String
concatFailures
  = foldr (\e s ->
      case e 
        of Fail f -> f ++ "\n" ++ s
           _ -> s)
      ""
