# Easter reference data

These files are reference material for testing `Data.Time.Calendar.Easter`.

Sources:

* Gregorian Easter dates: U.S. Census Bureau, "Easter Dates from 1600 to
  2099", including the downloadable `easter500.txt` ASCII file.
  URL: https://www.census.gov/data/software/x13as/genhol/easter-dates.html
  Raw file URL:
  https://www2.census.gov/software/x-13arima-seats/win-genhol/download/easter500.txt
* Gregorian ecclesiastical full moon dates: The Episcopal Church 1979 Book of
  Common Prayer, "Tables and Rules for Finding the Date of Easter Day" and
  "The Calendar of the Church Year". The BCP says the Golden Numbers in the
  present calendar are applicable from A.D. 1900 to A.D. 2099.
  URL: https://www.bcponline.org/Misc/tables.html
  URL: https://www.bcponline.org/General/calendar.html
* Orthodox Paschal full moon dates: OrthodoxWiki, "Paschalion". The table gives
  Julian Paschal Full Moon dates by 19-year cycle and Gregorian equivalents
  valid from 1900 to 2099; the same page states that Pascha is always the
  Sunday following the Paschal Full Moon.
  URL: https://orthodoxwiki.org/Paschalion

Files:

* `easter500.txt` from the U.S. Census Bureau

* `easter-reference-1900-2099.csv` normalizes the compact tables to year rows.
  Gregorian Easter dates come from the Census table; Gregorian Paschal Full
  Moon dates come from the BCP Golden Number table; Orthodox Paschal Full Moon
  dates come from the OrthodoxWiki table; Orthodox Easter dates are derived by
  taking the first Sunday strictly after the Orthodox Paschal Full Moon.
