module UnitTyped.SI.Derived.Time where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import Data.Ratio

----
-- Time
----

data Hour

instance Convertable TimeDimension Hour where
	factor _ = 3600
	showunit _ _ = "h"


data Minute

instance Convertable TimeDimension Minute where
	factor _ = 60
	showunit _ _ = "min."


data Day

instance Convertable TimeDimension Day where
	factor _ = 86400
	showunit _ _ = "day"


data Year

instance Convertable TimeDimension Year where
	factor _ = 365.2425 * 24 * 60 * 60
	showunit _ _ = "yr"


data Month

instance Convertable TimeDimension Month where
	factor _ = (365.2425 * 24 * 60 * 60) / 12
	showunit _ _ = "month"


data JulianYear

instance Convertable TimeDimension JulianYear where
	factor _ = 31557600
	showunit _ _ = "a"


data Hertz

instance Convertable (UnitCons Time (Neg One) UnitNil) Hertz where
	factor _ = 1
	showunit _ _ = "Hz"

--

minute :: (Fractional f) => Value f TimeDimension Minute
minute = one

hour :: (Fractional f) => Value f TimeDimension Hour
hour = one

day :: (Fractional f) => Value f TimeDimension Day
day = one

year :: (Fractional f) => Value f TimeDimension Year
year = one

julianyear :: (Fractional f) => Value f TimeDimension JulianYear
julianyear = one

month :: (Fractional f) => Value f TimeDimension Month
month = one

hertz :: (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Hertz
hertz = one