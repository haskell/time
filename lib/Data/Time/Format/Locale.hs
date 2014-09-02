-- Note: this file derives from old-locale:System.Locale.hs, which is copyright (c) The University of Glasgow 2001

module Data.Time.Format.Locale (

    TimeLocale(..)

    , defaultTimeLocale

    , iso8601DateFormat
    , rfc822DateFormat
    )
where

import Data.Time.LocalTime

data TimeLocale = TimeLocale {
        -- |full and abbreviated week days
        wDays  :: [(String, String)],
        -- |full and abbreviated months
        months :: [(String, String)],
        intervals :: [(String, String)],
        -- |AM\/PM symbols
        amPm   :: (String, String),
        -- |formatting strings
        dateTimeFmt, dateFmt,
        timeFmt, time12Fmt :: String,
        -- |time zones known by name
        knownTimeZones :: [TimeZone]
        } deriving (Eq, Ord, Show)

defaultTimeLocale :: TimeLocale
defaultTimeLocale =  TimeLocale {
        wDays  = [("Sunday",   "Sun"),  ("Monday",    "Mon"),
                  ("Tuesday",  "Tue"),  ("Wednesday", "Wed"),
                  ("Thursday", "Thu"),  ("Friday",    "Fri"),
                  ("Saturday", "Sat")],

        months = [("January",   "Jan"), ("February",  "Feb"),
                  ("March",     "Mar"), ("April",     "Apr"),
                  ("May",       "May"), ("June",      "Jun"),
                  ("July",      "Jul"), ("August",    "Aug"),
                  ("September", "Sep"), ("October",   "Oct"),
                  ("November",  "Nov"), ("December",  "Dec")],

        intervals = [ ("year","years")
                    , ("month", "months")
                    , ("day","days")
                    , ("hour","hours")
                    , ("min","mins")
                    , ("sec","secs")
                    , ("usec","usecs")
                    ],

        amPm = ("AM", "PM"),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%m/%d/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p",
        knownTimeZones = fmap (\(name,(minutes,dst)) -> TimeZone minutes dst name) _TIMEZONES_
        }

readTzOffset :: String -> Int
readTzOffset str =
    case str of
      (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
      (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
      _ -> 0
    where calc s h1 h2 m1 m2 = sign * (60 * h + m)
              where sign = if s == '-' then -1 else 1
                    h = read [h1,h2]
                    m = read [m1,m2]

-- Dubious
_TIMEZONES_ :: [(String, (Int, Bool))]
_TIMEZONES_ =
    -- New Zealand Daylight-Saving Time
    [("NZDT",    (readTzOffset "+13:00", True))
    -- International Date Line, East
    ,("IDLE",    (readTzOffset "+12:00", False))
    -- New Zealand Standard Time
    ,("NZST",    (readTzOffset "+12:00", False))
    -- New Zealand Time
    ,("NZT",     (readTzOffset "+12:00", False))
    -- Australia Eastern Summer Standard Time
    ,("AESST",   (readTzOffset "+11:00", False))
    -- Central Australia Summer Standard Time
    ,("ACSST",   (readTzOffset "+10:30", False))
    -- Central Australia Daylight-Saving Time
    ,("CADT",    (readTzOffset "+10:30", True))
    -- South Australian Daylight-Saving Time
    ,("SADT",    (readTzOffset "+10:30", True))
    -- Australia Eastern Standard Time
    ,("AEST",    (readTzOffset "+10:00", False))
    -- East Australian Standard Time
    ,("EAST",    (readTzOffset "+10:00", False))
    -- Guam Standard Time, Russia zone 9
    ,("GST",     (readTzOffset "+10:00", False))
    -- Melbourne, Australia
    ,("LIGT",    (readTzOffset "+10:00", False))
    -- South Australia Standard Time
    ,("SAST",    (readTzOffset "+09:30", False))
    -- Central Australia Standard Time
    ,("CAST",    (readTzOffset "+09:30", False))
    -- Australia Western Summer Standard Time
    ,("AWSST",   (readTzOffset "+09:00", False))
    -- Japan Standard Time, Russia zone 8
    ,("JST",     (readTzOffset "+09:00", False))
    -- Korea Standard Time
    ,("KST",     (readTzOffset "+09:00", False))
    -- Kwajalein Time
    ,("MHT",     (readTzOffset "+09:00", False))
    -- West Australian Daylight-Saving Time
    ,("WDT",     (readTzOffset "+09:00", True))
    -- Moluccas Time
    ,("MT",      (readTzOffset "+08:30", False))
    -- Australia Western Standard Time
    ,("AWST",    (readTzOffset "+08:00", False))
    -- China Coastal Time
    ,("CCT",     (readTzOffset "+08:00", False))
    -- West Australian Daylight-Saving Time
    ,("WADT",    (readTzOffset "+08:00", True))
    -- West Australian Standard Time
    ,("WST",     (readTzOffset "+08:00", False))
    -- Java Time
    ,("JT",      (readTzOffset "+07:30", False))
    -- Almaty Summer Time
    ,("ALMST",   (readTzOffset "+07:00", False))
    -- West Australian Standard Time
    ,("WAST",    (readTzOffset "+07:00", False))
    -- Christmas (Island) Time
    ,("CXT",     (readTzOffset "+07:00", False))
    -- Myanmar Time
    ,("MMT",     (readTzOffset "+06:30", False))
    -- Almaty Time
    ,("ALMT",    (readTzOffset "+06:00", False))
    -- Mawson (Antarctica) Time
    ,("MAWT",    (readTzOffset "+06:00", False))
    -- Indian Chagos Time
    ,("IOT",     (readTzOffset "+05:00", False))
    -- Maldives Island Time
    ,("MVT",     (readTzOffset "+05:00", False))
    -- Kerguelen Time
    ,("TFT",     (readTzOffset "+05:00", False))
    -- Afghanistan Time
    ,("AFT",     (readTzOffset "+04:30", False))
    -- Antananarivo Summer Time
    ,("EAST",    (readTzOffset "+04:00", False))
    -- Mauritius Island Time
    ,("MUT",     (readTzOffset "+04:00", False))
    -- Reunion Island Time
    ,("RET",     (readTzOffset "+04:00", False))
    -- Mahe Island Time
    ,("SCT",     (readTzOffset "+04:00", False))
    -- Iran Time
    ,("IRT",     (readTzOffset "+03:30", False))
    -- Iran Time
    ,("IT",      (readTzOffset "+03:30", False))
    -- Antananarivo, Comoro Time
    ,("EAT",     (readTzOffset "+03:00", False))
    -- Baghdad Time
    ,("BT",      (readTzOffset "+03:00", False))
    -- Eastern Europe Daylight-Saving Time
    ,("EETDST",  (readTzOffset "+03:00", True))
    -- Hellas Mediterranean Time (?)
    ,("HMT",     (readTzOffset "+03:00", False))
    -- British Double Summer Time
    ,("BDST",    (readTzOffset "+02:00", False))
    -- Central European Summer Time
    ,("CEST",    (readTzOffset "+02:00", False))
    -- Central European Daylight-Saving Time
    ,("CETDST",  (readTzOffset "+02:00", True))
    -- Eastern European Time, Russia zone 1
    ,("EET",     (readTzOffset "+02:00", False))
    -- French Winter Time
    ,("FWT",     (readTzOffset "+02:00", False))
    -- Israel Standard Time
    ,("IST",     (readTzOffset "+02:00", False))
    -- Middle European Summer Time
    ,("MEST",    (readTzOffset "+02:00", False))
    -- Middle Europe Daylight-Saving Time
    ,("METDST",  (readTzOffset "+02:00", True))
    -- Swedish Summer Time
    ,("SST",     (readTzOffset "+02:00", False))
    -- British Summer Time
    ,("BST",     (readTzOffset "+01:00", False))
    -- Central European Time
    ,("CET",     (readTzOffset "+01:00", False))
    -- Dansk Normal Tid
    ,("DNT",     (readTzOffset "+01:00", False))
    -- French Summer Time
    ,("FST",     (readTzOffset "+01:00", False))
    -- Middle European Time
    ,("MET",     (readTzOffset "+01:00", False))
    -- Middle European Winter Time
    ,("MEWT",    (readTzOffset "+01:00", False))
    -- Mitteleuropaeische Zeit
    ,("MEZ",     (readTzOffset "+01:00", False))
    -- Norway Standard Time
    ,("NOR",     (readTzOffset "+01:00", False))
    -- Seychelles Time
    ,("SET",     (readTzOffset "+01:00", False))
    -- Swedish Winter Time
    ,("SWT",     (readTzOffset "+01:00", False))
    -- Western European Daylight-Saving Time
    ,("WETDST",  (readTzOffset "+01:00", True))
    --  Greenwich Mean Time
    ,("GMT",     (readTzOffset "+00:00", False))
    --  Universal Time
    ,("UT",      (readTzOffset "+00:00", False))
    --  Universal Coordinated Time
    ,("UTC",     (readTzOffset "+00:00", False))
    --  Same as UTC
    ,("ZULU",    (readTzOffset "+00:00", False))
    --  Western European Time
    ,("WET",     (readTzOffset "+00:00", False))
    -- West Africa Time
    ,("WAT",     (readTzOffset "-01:00", False))
    -- Fernando de Noronha Summer Time
    ,("FNST",    (readTzOffset "-01:00", False))
    -- Fernando de Noronha Time
    ,("FNT",     (readTzOffset "-02:00", False))
    -- Brasilia Summer Time
    ,("BRST",    (readTzOffset "-02:00", False))
    -- Newfoundland Daylight-Saving Time
    ,("NDT",     (readTzOffset "-02:30", True))
    -- Atlantic Daylight-Saving Time
    ,("ADT",     (readTzOffset "-03:00", True))
    -- (unknown)
    ,("AWT",     (readTzOffset "-03:00", False))
    -- Brasilia Time
    ,("BRT",     (readTzOffset "-03:00", False))
    -- Newfoundland Standard Time
    ,("NFT",     (readTzOffset "-03:30", False))
    -- Newfoundland Standard Time
    ,("NST",     (readTzOffset "-03:30", False))
    -- Atlantic Standard Time (Canada)
    ,("AST",     (readTzOffset "-04:00", False))
    -- Atlantic/Porto Acre Summer Time
    ,("ACST",    (readTzOffset "-04:00", False))
    -- Eastern Daylight-Saving Time
    ,("EDT",     (readTzOffset "-04:00", True))
    -- Atlantic/Porto Acre Standard Time
    ,("ACT",     (readTzOffset "-05:00", False))
    -- Central Daylight-Saving Time
    ,("CDT",     (readTzOffset "-05:00", True))
    -- Eastern Standard Time
    ,("EST",     (readTzOffset "-05:00", False))
    -- Central Standard Time
    ,("CST",     (readTzOffset "-06:00", False))
    -- Mountain Daylight-Saving Time
    ,("MDT",     (readTzOffset "-06:00", True))
    -- Mountain Standard Time
    ,("MST",     (readTzOffset "-07:00", False))
    -- Pacific Daylight-Saving Time
    ,("PDT",     (readTzOffset "-07:00", True))
    -- Alaska Daylight-Saving Time
    ,("AKDT",    (readTzOffset "-08:00", True))
    -- Pacific Standard Time
    ,("PST",     (readTzOffset "-08:00", False))
    -- Yukon Daylight-Saving Time
    ,("YDT",     (readTzOffset "-08:00", True))
    -- Alaska Standard Time
    ,("AKST",    (readTzOffset "-09:00", False))
    -- Hawaii/Alaska Daylight-Saving Time
    ,("HDT",     (readTzOffset "-09:00", True))
    -- Yukon Standard Time
    ,("YST",     (readTzOffset "-09:00", False))
    -- Marquesas Time
    ,("MART",    (readTzOffset "-09:30", False))
    -- Alaska/Hawaii Standard Time
    ,("AHST",    (readTzOffset "-10:00", False))
    -- Hawaii Standard Time
    ,("HST",     (readTzOffset "-10:00", False))
    -- Central Alaska Time
    ,("CAT",     (readTzOffset "-10:00", False))
    -- Nome Time
    ,("NT",      (readTzOffset "-11:00", False))
    -- International Date Line, West
    ,("IDLW",    (readTzOffset "-12:00", False))
    ]

{- | Construct format string according to <http://en.wikipedia.org/wiki/ISO_8601 ISO-8601>.

The @Maybe String@ argument allows to supply an optional time specification. E.g.:

@
'iso8601DateFormat' Nothing            == "%Y-%m-%d"           -- i.e. @/YYYY-MM-DD/@
'iso8601DateFormat' (Just "%H:%M:%S")  == "%Y-%m-%dT%H:%M:%S"  -- i.e. @/YYYY-MM-DD/T/HH:MM:SS/@
@
-}

iso8601DateFormat :: Maybe String -> String
iso8601DateFormat mTimeFmt =
    "%Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> 'T' : fmt

-- | Format string according to <http://tools.ietf.org/html/rfc822#section-5 RFC822>.
rfc822DateFormat :: String
rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"
