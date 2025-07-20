# Change Log

## [1.15] - Unreleased

- support compiler / GHC backends (with CI):
  - JavaScript
  - WebAssembly
  - MicroHs
- add diffTimeOfDay
- add \[add/diff\]\[UTC/Local\]Duration\[Clip/Rollover\]
- UNIXish formatting/parsing:
  - add instance ParseTime DayOfWeek
  - make use of %s specifiers in parsing various types
  - instances for Quarter and QuarterOfYear
  - %v specifier for quarter-of-year
- ISO8601 formatting/parsing:
  - loosen up parsing time-zone modifiers
- add Lift instances to all types (really this time)
- hide Data.Time.Format.Internal

## [1.14] - 2024-03-10
- add Lift instances to all types
- add Generic instances to all types that have exposed constructors
- fix show of CalendarDiffTime
- fix diffGregorianDurationRollOver, diffJulianDurationRollOver
- Parsing is now maximal munch rather than ambiguous for
  - digits of %q and %Q specifiers
  - optional timezone for UTCTime
  - optional specifiers in ISO8601 formats

## [1.12.2] - 2022-05-14
- add weekFirstDay, weekLastDay, weekAllDays
- expose formatting/parsing internals
- fix: handle +HH format for ISO8601 timeOffsetFormat etc.
- fix clock_REALTIME for WebAssembly

## [1.12.1] - 2021-10-24
- add DayPeriod class for periods of days
- add QuarterDay pattern and DayOfQuarter type synonym
- add CommonEra and BeforeCommonEra patterns

## [1.12] - 2021-06-12
- support GHC 8.8, 8.10, 9.0 only
- add patterns for each month of year
- fix: don't provide TAI clock where it's unavailable (e.g. FreeBSD)
- fix: handle time of day 24:00:00 for ISO 8601 parsing (only)
- fix parsing of %f and %G with negative years

## [1.11.1.2] - 2021-04-24
- fix cabal file
- correct "license" field in cabal file
- add dates to changelog entries

## [1.11.1.1] - 2020-12-09
- fix module Safe status

## [1.11.1] - 2020-11-23
- all modules Safe or Trustworthy
- fix NFData instances for DiffTime, NominalDiffTime, TimeOfDay
- add missing Ix, Enum, NFData instances to DayOfWeek, CalendarDiffDays, CalendarDiffTime, Month, Quarter, QuarterOfYear

## [1.11] - 2020-10-14
- new calendrical type synonyms and abstract constructors
- new Month type, with appropriate functions
- new QuarterOfYear and Quarter type, with appropriate functions
- new functions for working with week-based years
- new parseTimeMultipleM function for a list of (format, input) pairs
- add instance Ord DayOfWeek
- add instance Read DiffTime (and NominalDiffTime)
- change instance Read UTCTime to allow omitted timezone
- parsing dates rejects ambiguity based on digits, even if there's only one valid date

## [1.10] - 2020-03-13
- remove deprecated functions parseTime, readTime, readsTime
- deprecate iso8601DateFormat
- parsing: fix %_Q %-Q %_q %-q
- parsing: fix parsing of BCE years
- formatting: fix %3ES %3Es
- change internal members of ParseTime to allow newtype-deriving
- new functions (aliases) pastMidnight & sinceMidnight

## [1.9.3] - 2019-05-20
- documentation fixes

## [1.9.2] - 2018-08-01
- add Data and Typeable instance for CalendarDiffDays and CalendarDiffTime
- "@since" annotations for everything after 1.9
- fix import issue with GHC 8.6

## [1.9.1] - 2018-02-27
- new functions secondsToNominalDiffTime & nominalDiffTimeToSeconds
- expose FormatTime and ParseTime in Data.Time.Format.Internal

## [1.9] - 2018-01-25
- new conversion functions timeToDaysAndTimeOfDay & daysAndTimeOfDayToTime
- new DayOfWeek type
- new CalendarDiffDays and CalendarDiffTime types
- new ISO8601 module for ISO 8601 formatting & parsing
- new addLocalTime, diffLocalTime
- hide members of FormatTime and ParseTime classes
- formatting & parsing for diff types (NominalDiffTime, DiffTime, CalendarDiffDays, CalendarDiffTime)
- formatting: %Ez and %EZ for ±HH:MM format
- parseTimeM: use MonadFail constraint when supported
- parsing: reject invalid (and empty) time-zones with %z and %Z
- parsing: reject invalid hour/minute/second specifiers

## [1.8.0.4] - 2018-01-09
- Fix "show minBound" bug
- haddock: example for parseTimeM

## [1.8.0.3] - 2017-08-04
- Add "Quick start" documentation

## [1.8.0.2] - 2017-05-13
- Fix behaviour of %Q in format

## [1.8.0.1] - 2017-03-11
- Get building on 32 bit machine

## [1.8] - 2017-02-14
- Added SystemTime
- Data.Time.Format: allow padding widths in specifiers for formatting (but not parsing)
- Test: use tasty, general clean-up
- Test: separate out UNIX-specific tests, so the others can be run on Windows
- Clean up haddock.

## [1.7.0.1] - 2016-12-19
- Fix bounds issue in .cabal file

## [1.7] - 2016-11-19
- Data.Time.Clock.TAI: change LeapSecondTable to LeapSecondMap with Maybe type; remove parseTAIUTCDATFile

## [1.6.0.1] - 2016-05-07
- Get building with earlier GHC versions
- Set lower bound of base correctly

## [1.6] - 2015-12-20

### Added
- FormatTime, ParseTime, Show and Read instances for UniversalTime
- diffTimeToPicoseconds
- this change log

### Changed
- Use clock_gettime where available
- Read and Show instances exported in the same module as their types
- Fixed bug in fromSundayStartWeekValid
- Parsing functions now reject invalid dates
- Various documentation fixes

## [1.5.0.1] - 2014-12-13

## [1.5] - 2014-09-10

## [1.4.2] - 2014-03-03

## [1.4.1] - 2013-06-24

## [1.4.0.2] - 2012-11-25

## [1.4.0.1] - 2011-10-31

## [1.4] - 2011-09-13

## [1.3] - 2011-08-10

## [1.2.0.5] - 2011-05-11

## [1.2.0.4] - 2011-02-03

## [1.2.0.3] - 2010-06-22

## [1.2.0.2] - 2010-04-26

## [1.2.0.1] - 2010-04-11

## [1.2] - 2010-04-11

## [1.1.4] - 2009-07-17

## [1.1.3] - 2009-06-01

## [1.1.2.4] - 2009-04-17

## [1.1.2.3] - 2009-01-17

## [1.1.2.2] - 2008-10-11

## [1.1.2.1] - 2008-06-19

## [1.1.2.0] - 2007-11-03

## [1.1.1] - 2007-04-22

## [1.0] - 2006-11-02
