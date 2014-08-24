-- Note: this file derives from old-locale:System.Locale.hs, which is copyright (c) The University of Glasgow 2001

module Data.Time.Format.Locale (

    TimeLocale(..)

    , defaultTimeLocale

    , iso8601DateFormat
    , rfc822DateFormat
    )
where

import Prelude

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
        timeFmt, time12Fmt :: String
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
        time12Fmt = "%I:%M:%S %p"
        }


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
