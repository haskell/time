{-# LANGUAGE ForeignFunctionInterface #-}

module Test.LocalTime.TimeZone (
    testTimeZone,
) where

import Data.Time
import Foreign
import Foreign.C
import System.Environment (setEnv)
import Test.Tasty
import Test.Tasty.HUnit

type GetCurrentTimezoneSeconds =
    CTime -> Ptr CInt -> Ptr CString -> IO CLong

foreign import ccall unsafe "test_time_zone_fallback_set_state"
    test_time_zone_fallback_set_state :: CLong -> CLong -> CInt -> IO ()

foreign import ccall unsafe "test_get_current_timezone_seconds_no_altzone"
    test_get_current_timezone_seconds_no_altzone :: GetCurrentTimezoneSeconds

foreign import ccall unsafe "test_get_current_timezone_seconds_altzone"
    test_get_current_timezone_seconds_altzone :: GetCurrentTimezoneSeconds

testTimeZone :: TestTree
testTimeZone =
    testGroup
        "TimeZone"
        [ testCase "getTimeZone respects TZ env var" $ do
            let
                epoch = UTCTime (ModifiedJulianDay 57000) 0
            setEnv "TZ" "UTC+0"
            zone1 <- getTimeZone epoch
            setEnv "TZ" "EST+5"
            zone2 <- getTimeZone epoch
            assertBool "zone not changed" $ zone1 /= zone2
        , testGroup
            "get_current_timezone_seconds fallback"
            [ testFallback
                "without altzone, standard time"
                test_get_current_timezone_seconds_no_altzone
                18000
                14400
                0
                (-18000)
                "EST"
            , testFallback
                "without altzone, daylight time"
                test_get_current_timezone_seconds_no_altzone
                18000
                14400
                1
                (-14400)
                "EDT"
            , testFallback
                "with altzone, standard time"
                test_get_current_timezone_seconds_altzone
                18000
                14400
                0
                (-18000)
                "EST"
            , testFallback
                "with altzone, daylight time"
                test_get_current_timezone_seconds_altzone
                18000
                14400
                1
                (-14400)
                "EDT"
            ]
        ]

testFallback ::
    String ->
    GetCurrentTimezoneSeconds ->
    CLong ->
    CLong ->
    CInt ->
    CLong ->
    String ->
    TestTree
testFallback name getTimezoneSeconds timezoneSeconds altzoneSeconds isDst expectedSeconds expectedName =
    testCase name $
        with 0 $ \pdst ->
            with nullPtr $ \pcname -> do
                test_time_zone_fallback_set_state timezoneSeconds altzoneSeconds isDst
                seconds <- getTimezoneSeconds 0 pdst pcname
                dst <- peek pdst
                cname <- peek pcname
                name' <- peekCString cname
                assertEqual "seconds" expectedSeconds seconds
                assertEqual "dst" isDst dst
                assertEqual "name" expectedName name'
