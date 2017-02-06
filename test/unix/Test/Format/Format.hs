{-# LANGUAGE ForeignFunctionInterface #-}

module Test.Format.Format(testFormat) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Char
import Foreign
import Foreign.C
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.Tasty
import Test.TestUtil
import System.IO.Unsafe

{-
    size_t format_time (
    char *s, size_t maxsize,
    const char *format,
    int isdst,int gmtoff,time_t t);
-}

foreign import ccall unsafe "FormatStuff.h format_time" format_time :: CString -> CSize -> CString -> CInt -> CInt -> CString -> CTime -> IO CSize

withBuffer :: Int -> (CString -> IO CSize) -> IO String
withBuffer n f = withArray (replicate n 0) (\buffer -> do
            len <- f buffer
            peekCStringLen (buffer,fromIntegral len)
        )

unixFormatTime :: String -> TimeZone -> UTCTime -> String
unixFormatTime fmt zone time = unsafePerformIO $ withCString fmt (\pfmt -> withCString (timeZoneName zone) (\pzonename ->
        withBuffer 100 (\buffer -> format_time buffer 100 pfmt
                (if timeZoneSummerOnly zone then 1 else 0)
                (fromIntegral (timeZoneMinutes zone * 60))
                pzonename
                (fromInteger (truncate (utcTimeToPOSIXSeconds time)))
            )
        ))

locale :: TimeLocale
locale = defaultTimeLocale {dateTimeFmt = "%a %b %e %H:%M:%S %Y"}

zones :: Gen TimeZone
zones = do
    mins <- choose (-2000,2000)
    dst <- arbitrary
    name <- return "ZONE"
    return $ TimeZone mins dst name

times :: Gen UTCTime
times = do
    day <- choose (-25000,75000)
    time <- return midnight
    return $ localTimeToUTC utc $ LocalTime (ModifiedJulianDay day) time

padN :: Int -> Char -> String -> String
padN n _ s | n <= (length s) = s
padN n c s = (replicate (n - length s) c) ++ s

unixWorkarounds :: String -> String -> String
unixWorkarounds "%_Y" s = padN 4 ' ' s
unixWorkarounds "%0Y" s = padN 4 '0' s
unixWorkarounds "%_C" s = padN 2 ' ' s
unixWorkarounds "%0C" s = padN 2 '0' s
unixWorkarounds "%_G" s = padN 4 ' ' s
unixWorkarounds "%0G" s = padN 4 '0' s
unixWorkarounds "%_f" s = padN 2 ' ' s
unixWorkarounds "%0f" s = padN 2 '0' s
unixWorkarounds _ s = s

compareFormat :: (String -> String) -> String -> TimeZone -> UTCTime -> Result
compareFormat modUnix fmt zone time = let
    ctime = utcToZonedTime zone time
    haskellText = formatTime locale fmt ctime
    unixText = unixFormatTime fmt zone time
    expectedText = unixWorkarounds fmt (modUnix unixText)
    in assertEqualQC "" expectedText haskellText

-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- f not supported
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

-- as found in "man strftime" on a glibc system. '#' is different, though
modifiers :: [String]
modifiers = ["","_","-","0","^"]

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"]
 ++ (do
    char <- chars
    modifier <- modifiers
    return $ "%" ++ modifier ++ [char]
    )

hashformats :: [String]
hashformats = do
    char <- chars
    return $ "%#"++[char]

testCompareFormat :: [TestTree]
testCompareFormat = tgroup formats $ \fmt -> do
    time <- times
    zone <- zones
    return $ compareFormat id fmt zone time

testCompareHashFormat :: [TestTree]
testCompareHashFormat = tgroup hashformats $ \fmt -> do
    time <- times
    zone <- zones
    return $ compareFormat (fmap toLower) fmt zone time

testFormat :: TestTree
testFormat = testGroup "testFormat" $ testCompareFormat ++ testCompareHashFormat
