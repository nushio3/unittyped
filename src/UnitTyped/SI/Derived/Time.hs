{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- |Units derived from the SI unit for time: 'second'.
module UnitTyped.SI.Derived.Time where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import Data.Typeable

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
	deriving Typeable

instance Convertible Hour where
	factor _ = 3600
	showunit _ = "h"
        type DimensionOf Hour = TimeDimension 

-- |Minute (min.).
data Minute
	deriving Typeable

instance Convertible Minute where
	factor _ = 60
	showunit _ = "min."
        type DimensionOf Minute = TimeDimension 

-- |Day (day).
data Day
	deriving Typeable

instance Convertible Day where
	factor _ = 86400
	showunit _ = "day"
        type DimensionOf Day = TimeDimension 


-- |Year (yr). This is an average year in the Gregorian calender (so 365.2425 days).
data Year
	deriving Typeable

instance Convertible Year where
	factor _ = 365.2425 * 24 * 60 * 60
	showunit _ = "yr"
        type DimensionOf Year = TimeDimension 

-- |Month (month). Average length of a month (365.2425 / 12).
data Month
	deriving Typeable

instance Convertible Month where
	factor _ = (365.2425 * 24 * 60 * 60) / 12
	showunit _ = "month"
        type DimensionOf Month = TimeDimension 

-- |Julian year (a). This is an average year in the Julian calender (so 365.25 days). Still used in astronomy.
data JulianYear
	deriving Typeable

instance Convertible JulianYear where
	factor _ = 31557600
	showunit _ = "a"
        type DimensionOf JulianYear = TimeDimension 

-- |Frequency in Hertz. (Hz)
data Hertz
	deriving Typeable

instance Convertible Hertz where
	factor _ = 1
	showunit _ = "Hz"
        type DimensionOf Hertz = '[ '(Time, Neg One) ]

--

-- |One minute (min.).
minute :: (Fractional f) => Value TimeDimension (U Minute) f
minute = one

-- |One hour (h).
hour :: (Fractional f) => Value TimeDimension (U Hour) f
hour = one

-- |One day (day).
day :: (Fractional f) => Value TimeDimension (U Day) f
day = one

-- |One year (yr).
year :: (Fractional f) => Value TimeDimension (U Year) f
year = one

-- |One Julian year (a).
julian_year :: (Fractional f) => Value TimeDimension (U JulianYear) f
julian_year = one

-- |One month (month).
month :: (Fractional f) => Value TimeDimension (U Month) f
month = one

-- |One herz (Hz).
hertz :: (Fractional f) => Value '[ '(Time, NOne) ] (U Hertz) f
hertz = one

--

-- Interaction with Data.Time

-- |Convert a 'DT.DiffTime' into a value in seconds.
fromDiffTime :: (Fractional f) => DT.DiffTime -> Value TimeDimension (U Second) f
fromDiffTime = mkVal . fromRational . toRational

-- |Convert a 'DTC.NominalDiffTime' into a value in seconds.
fromNominalDiffTime :: (Fractional f) => DTC.NominalDiffTime -> Value TimeDimension (U Second) f
fromNominalDiffTime = mkVal . fromRational . toRational

-- |Convert the number of seconds since a given 'DTC.UTCTime' into a value in seconds.
since :: (Fractional f) => DTC.UTCTime -> IO (Value TimeDimension (U Second) f)
since time = do { t <- DTC.getCurrentTime
		        ; return (fromNominalDiffTime (DTC.diffUTCTime t time) `as` second)
		        }

-- |Calculate the number of seconds since a given date and\/or time, parsed according to the first argument.
since_str :: (Fractional f) => String -> String -> IO (Value TimeDimension (U Second) f)
since_str fmt str = since (DTF.readTime SL.defaultTimeLocale fmt str)
