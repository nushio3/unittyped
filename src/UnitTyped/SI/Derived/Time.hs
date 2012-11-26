{-# LANGUAGE DataKinds #-}
-- |Units derived from the SI unit for time: 'second'.
module UnitTyped.SI.Derived.Time where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import Data.Ratio
import qualified Data.Time as DT
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Format as DTF
import qualified System.Locale as SL

----
-- Time
----


-- |Hour (h).
data Hour

instance Convertible TimeDimension Hour where
	factor _ = 3600
	showunit _ _ = "h"

-- |Minute (min.).
data Minute

instance Convertible TimeDimension Minute where
	factor _ = 60
	showunit _ _ = "min."

-- |Day (day).
data Day

instance Convertible TimeDimension Day where
	factor _ = 86400
	showunit _ _ = "day"

-- |Year (yr). This is an average year in the Gregorian calender (so 365.2425 days).
data Year

instance Convertible TimeDimension Year where
	factor _ = 365.2425 * 24 * 60 * 60
	showunit _ _ = "yr"

-- |Month (month). Average length of a month (365.2425 / 12).
data Month

instance Convertible TimeDimension Month where
	factor _ = (365.2425 * 24 * 60 * 60) / 12
	showunit _ _ = "month"

-- |Julian year (a). This is an average year in the Julian calender (so 365.25 days). Still used in astronomy.
data JulianYear

instance Convertible TimeDimension JulianYear where
	factor _ = 31557600
	showunit _ _ = "a"

-- |Frequency in Hertz. (Hz)
data Hertz

instance Convertible (UnitCons Time (Neg One) UnitNil) Hertz where
	factor _ = 1
	showunit _ _ = "Hz"

--

-- |One minute (min.).
minute :: (Fractional f) => Value f TimeDimension Minute
minute = one

-- |One hour (h).
hour :: (Fractional f) => Value f TimeDimension Hour
hour = one

-- |One day (day).
day :: (Fractional f) => Value f TimeDimension Day
day = one

-- |One year (yr).
year :: (Fractional f) => Value f TimeDimension Year
year = one

-- |One Julian year (a).
julian_year :: (Fractional f) => Value f TimeDimension JulianYear
julian_year = one

-- |One month (month).
month :: (Fractional f) => Value f TimeDimension Month
month = one

-- |One herz (Hz).
hertz :: (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Hertz
hertz = one

--

-- Interaction with Data.Time

-- |Convert a 'DT.DiffTime' into a value in seconds.
fromDiffTime :: (Fractional f) => DT.DiffTime -> Value f TimeDimension Second
fromDiffTime = mkVal . fromRational . toRational

-- |Convert a 'DTC.NominalDiffTime' into a value in seconds.
fromNominalDiffTime :: (Fractional f) => DTC.NominalDiffTime -> Value f TimeDimension Second
fromNominalDiffTime = mkVal . fromRational . toRational

-- |Convert the number of seconds since a given 'DTC.UTCTime' into a value in seconds.
since :: (Fractional f) => DTC.UTCTime -> IO (Value f TimeDimension Second)
since time = do { t <- DTC.getCurrentTime
		        ; return (fromNominalDiffTime (DTC.diffUTCTime t time) `as` second)
		        }

-- |Calculate the number of seconds since a given date and\/or time, parsed according to the first argument.
since_str :: (Fractional f) => String -> String -> IO (Value f TimeDimension Second)
since_str fmt str = since (DTF.readTime SL.defaultTimeLocale fmt str)
