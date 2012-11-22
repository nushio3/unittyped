module UnitTyped.SI.Derived.Time where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import Data.Ratio

----
-- Time
----

data Hour

instance Convertable TimeUnit Hour where
	factor _ = 3600
	showunit _ _ = "h"


data Minute

instance Convertable TimeUnit Minute where
	factor _ = 60
	showunit _ _ = "min."


data Day

instance Convertable TimeUnit Day where
	factor _ = 86400
	showunit _ _ = "day"


data Year

instance Convertable TimeUnit Year where
	factor _ = 365.2425 * 24 * 60 * 60
	showunit _ _ = "yr"


data Month

instance Convertable TimeUnit Month where
	factor _ = (365.2425 * 24 * 60 * 60) / 12
	showunit _ _ = "month"


data JulianYear

instance Convertable TimeUnit JulianYear where
	factor _ = 31557600
	showunit _ _ = "a"


data Herz

instance Convertable (UnitCons Time (Neg One) UnitNil) Herz where
	factor _ = 1
	showunit _ _ = "Hz"

--

minute :: (Fractional f) => Value f TimeUnit Minute
minute = one

hour :: (Fractional f) => Value f TimeUnit Hour
hour = one

day :: (Fractional f) => Value f TimeUnit Day
day = one

year :: (Fractional f) => Value f TimeUnit Year
year = one

julianyear :: (Fractional f) => Value f TimeUnit JulianYear
julianyear = one

month :: (Fractional f) => Value f TimeUnit Month
month = one

herz :: (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Herz
herz = one