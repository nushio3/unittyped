module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

import Data.Ratio

---
-- Count
---

data Percentage
type Percentages = (Fractional f) => Value f CountUnit Percentage

instance Convertable CountUnit Percentage where
	factor _ = 0.01
	showunit _ _ = "%"

data Permil
type Permils = (Fractional f) => Value f CountUnit Permil

instance Convertable CountUnit Permil where
	factor _ = 0.001
	showunit _ _ = "‰"

data Ppm
type Ppms = (Fractional f) => Value f CountUnit Ppm

instance Convertable CountUnit Ppm where
	factor _ = 0.1^6
	showunit _ _ = "ppm"

data Ppb
type Ppbs = (Fractional f) => Value f CountUnit Ppb

instance Convertable CountUnit Ppb where
	factor _ = 0.1^9
	showunit _ _ = "ppb"

data Ppt
type Ppts = (Fractional f) => Value f CountUnit Ppt

instance Convertable CountUnit Ppt where
	factor _ = 0.1^12
	showunit _ _ = "ppt"

data Radian
type Radians = (Fractional f) => Value f CountUnit Radian

instance Convertable CountUnit Radian where
	factor _ = 1
	showunit _ _ = "rad"

data Degree
type Degrees = (Fractional f) => Value f CountUnit Degree

instance Convertable CountUnit Degree where
	factor _ = 3.141592653589793 / 180
	showunit _ _ = "°"

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

data Year
type Years = (Fractional f) => Value f TimeUnit Year

instance Convertable TimeUnit Year where
	factor _ = 365.2425 * 24 * 60 * 60
	showunit _ _ = "yr"

data Month
type Months = (Fractional f) => Value f TimeUnit Month

instance Convertable TimeUnit Month where
	factor _ = (365.2425 * 24 * 60 * 60) / 12
	showunit _ _ = "month"

data JulianYear
type JulianYears = (Fractional f) => Value f TimeUnit JulianYear

instance Convertable TimeUnit JulianYear where
	factor _ = 31557600
	showunit _ _ = "a"

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

type Charge = (UnitCons Time (Pos One) (UnitCons Current (Pos One) UnitNil))
data Coulomb

instance Convertable Charge Coulomb where
	factor _ = 1
	showunit _ _ = "C"

type Potential = (UnitCons Current (Neg One) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) (UnitCons Time (Neg (Suc (Suc One))) UnitNil))))
data Volt
type Volts = (Fractional f) => Value f Potential Volt

instance Convertable Potential Volt where
	factor _ = 1
	showunit _ _ = "V"

type Capacitance = (UnitCons Current (Pos (Suc One)) (UnitCons Mass (Neg One) (UnitCons Length (Neg (Suc One)) (UnitCons Time (Pos (Suc (Suc (Suc One)))) UnitNil))))
data Farad
type Farads = (Fractional f) => Value f Capacitance Farad

instance Convertable Capacitance Farad where
	factor _ = 1
	showunit _ _ = "F"

type Resistance = (UnitCons Current (Neg (Suc One)) (UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))))
data Ohm
type Ohms = (Fractional f) => Value f Resistance Ohm

instance Convertable Resistance Ohm where
	factor _ = 1
	showunit _ _ = "Ω"

type Conductance = (UnitCons Current (Pos (Suc One)) (UnitCons Mass (Neg One) (UnitCons Length (Neg (Suc One)) (UnitCons Time (Pos (Suc (Suc One))) UnitNil))))
data Siemens

instance Convertable Conductance Siemens where
	factor _ = 1
	showunit _ _ = "S"

type Flux = (UnitCons Current (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) (UnitCons Time (Neg (Suc One)) UnitNil))))
data Weber

instance Convertable Flux Weber where
	factor _ = 1
	showunit _ _ = "Wb"

type FluxDensity = (UnitCons Length (Neg One) (UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Current (Neg One) UnitNil))))
data Tesla

instance Convertable FluxDensity Tesla where
	factor _ = 1
	showunit _ _ = "T"

type Inductance = (UnitCons Current (Neg (Suc One)) (UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))))
data Henry

instance Convertable Inductance Henry where
	factor _ = 1
	showunit _ _ = "H"

--

percentage :: (Fractional f) => Value f CountUnit Percentage
percentage = one

permil :: (Fractional f) => Value f CountUnit Permil
permil = one

ppm :: (Fractional f) => Value f CountUnit Ppm
ppm = one

ppb :: (Fractional f) => Value f CountUnit Ppb
ppb = one

ppt :: (Fractional f) => Value f CountUnit Ppt
ppt = one

rad :: (Fractional f) => Value f CountUnit Radian
rad = one

deg :: (Fractional f) => Value f CountUnit Degree
deg = one

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

year :: (Fractional f) => Value f TimeUnit Year
year = one

julianyear :: (Fractional f) => Value f TimeUnit JulianYear
julianyear = one

month :: (Fractional f) => Value f TimeUnit Month
month = one

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

coulomb :: (Fractional f) => Value f Charge Coulomb
coulomb = one

volt :: (Fractional f) => Value f Potential Volt
volt = one

farad :: (Fractional f) => Value f Capacitance Farad
farad = one

ohm :: (Fractional f) => Value f Resistance Ohm
ohm = one

siemens :: (Fractional f) => Value f Conductance Siemens
siemens = one

weber :: (Fractional f) => Value f Flux Weber
weber = one

tesla :: (Fractional f) => Value f FluxDensity Tesla
tesla = one

henry :: (Fractional f) => Value f Inductance Henry
henry = one