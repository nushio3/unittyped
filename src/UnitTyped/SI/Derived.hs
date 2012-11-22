module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

---
-- Count
---

data Percentage
type Percentages = (Fractional f) => Value f LengthUnit Mile

instance Convertable CountUnit Percentage where
	factor _ = 0.01
	showunit _ _ = "%"

----
-- Length
----

type Kilometers = (Fractional f) => Value f LengthUnit (Kilo Meter)
type Milimeters = (Fractional f) => Value f LengthUnit (Mili Meter)

--

data Mile
type Miles = (Fractional f) => Value f LengthUnit Mile

instance Convertable LengthUnit Mile where
	factor _ = 1609
	showunit _ _ = "mile"
--

data Inch
type Inches = (Fractional f) => Value f LengthUnit Inch

instance Convertable LengthUnit Inch where
	factor _ = 0.0254
	showunit _ _ = "in"

--

data Yard
type Yards = (Fractional f) => Value f LengthUnit Yard

instance Convertable LengthUnit Yard where
	factor _ = 0.9144
	showunit _ _ = "yd"

--

data Foot
type Feet = (Fractional f) => Value f LengthUnit Foot

instance Convertable LengthUnit Foot where
	factor _ = 0.3048
	showunit _ _ = "ft"

data Joerimijl
type Joerimijlen = (Fractional f) => Value f LengthUnit Joerimijl

instance Convertable LengthUnit Joerimijl where
	factor _ = 20.1e3
	showunit _ _ = "jmi"

--

type VolumeUnit = UnitCons Length (Pos (Suc (Suc One))) UnitNil

data Liter
type Liters = (Fractional f) => Value f VolumeUnit Liter

instance Convertable VolumeUnit Liter where
	factor _ = 0.001
	showunit _ _ = "L"

data Gallon
type Gallons = (Fractional f) => Value f VolumeUnit Gallon

instance Convertable VolumeUnit Gallon where
	factor _ = 0.00454609
	showunit _ _ = "gallon"

----
-- Time
----

data Hour
type Hours = (Fractional f) => Value f TimeUnit Hour

instance Convertable TimeUnit Hour where
	factor _ = 3600
	showunit _ _ = "h"

--

data Minute
type Minutes = (Fractional f) => Value f TimeUnit Minute

instance Convertable TimeUnit Minute where
	factor _ = 60
	showunit _ _ = "min."

--

data Day
type Days = (Fractional f) => Value f TimeUnit Day

instance Convertable TimeUnit Day where
	factor _ = 86400
	showunit _ _ = "day"

--

data Herz
type Herzs = (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Herz

instance Convertable (UnitCons Time (Neg One) UnitNil) Herz where
	factor _ = 1
	showunit _ _ = "Hz"


----
-- Mass
----

type Kilograms = (Fractional f) => Value f MassUnit (Kilo Gram)

data PlanckMass
type PlanckMasses = (Fractional f) => Value f MassUnit PlanckMass

instance Convertable MassUnit PlanckMass where
	factor _ = 2.176513e-8
	showunit _ _ = "m_P"

----
-- Misc
----


type Speed = UnitCons Time (Neg One) (UnitCons Length (Pos One) UnitNil)
type Acceleration = UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos One) UnitNil)

type Force = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos One) UnitNil))
data Newton
type Newtons = (Fractional f) => Value f Force Newton

instance Convertable Force Newton where
	factor _ = 1
	showunit _ _ = "N"

type Energy = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))
data Joule
type Joules = (Fractional f) => Value f Energy Joule
type Kwh = (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)

instance Convertable Energy Joule where
	factor _ = 1
	showunit _ _ = "J"

data Ev
type Evs = (Fractional f) => Value f Energy Ev

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ _ = "eV"

type Power = UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))
data Watt
type Watts = (Fractional f) => Value f Power Watt

instance Convertable Power Watt where
	factor _ = 1
	showunit _ _ = "W"

type Pressure = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Neg One) UnitNil))
data Pascal
type Pascals = (Fractional f) => Value f Pressure Pascal

instance Convertable Pressure Pascal where
	factor _ = 1
	showunit _ _ = "Pa"

--

percentage :: (Fractional f) => Value f CountUnit Percentage
percentage = one

--

kilometer = kilo meter
milimeter = mili meter

mile :: (Fractional f) => Value f LengthUnit Mile
mile = one

inch :: (Fractional f) => Value f LengthUnit Inch
inch = one

yard :: (Fractional f) => Value f LengthUnit Yard
yard = one

foot :: (Fractional f) => Value f LengthUnit Foot
foot = one

joerimijl :: (Fractional f) => Value f LengthUnit Joerimijl
joerimijl = one

--

liter :: (Fractional f) => Value f VolumeUnit Liter
liter = one

gallon :: (Fractional f) => Value f VolumeUnit Gallon
gallon = one

--

minute :: (Fractional f) => Value f TimeUnit Minute
minute = one

hour :: (Fractional f) => Value f TimeUnit Hour
hour = one

day :: (Fractional f) => Value f TimeUnit Day
day = one

herz :: (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Herz
herz = one

--

kilogram :: (Fractional f) => Value f MassUnit (Kilo Gram)
kilogram = one

m_P :: (Fractional f) => Value f MassUnit PlanckMass
m_P = one

--

newton :: (Fractional f) => Value f Force Newton
newton = one

joule :: (Fractional f) => Value f Energy Joule
joule = one

eV :: (Fractional f) => Value f Energy Ev
eV = one

kwh :: (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)
kwh = one

watt :: (Fractional f) => Value f Power Watt
watt = one

pascal :: (Fractional f) => Value f Pressure Pascal
pascal = one