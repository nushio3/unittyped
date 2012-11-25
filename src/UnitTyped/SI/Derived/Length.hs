module UnitTyped.SI.Derived.Length where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived
import UnitTyped.SI.Meta

import Data.Ratio

----
-- Length
----

data Mile

instance Convertable LengthDimension Mile where
	factor _ = 1609.344
	showunit _ _ = "mile"


data Inch

instance Convertable LengthDimension Inch where
	factor _ = 0.0254
	showunit _ _ = "in"


data Yard

instance Convertable LengthDimension Yard where
	factor _ = 0.9144
	showunit _ _ = "yd"


data Foot

instance Convertable LengthDimension Foot where
	factor _ = 0.3048
	showunit _ _ = "ft"


data Joerimijl

instance Convertable LengthDimension Joerimijl where
	factor _ = 20.1e3
	showunit _ _ = "jmi"


data Ångström

instance Convertable LengthDimension Ångström where
	factor _ = 10e-10
	showunit _ _ = "Å"


data NauticalMile

instance Convertable LengthDimension NauticalMile where
	factor _ = 1852
	showunit _ _ = "M"

----
-- 2 dimensional
----

type AreaUnit = UnitCons Length (Pos (Suc One)) UnitNil


data Barn

instance Convertable AreaUnit Barn where
	factor _ = 1e-28
	showunit _ _ = "b"

----
-- 3 dimensional
----

type VolumeUnit = UnitCons Length (Pos (Suc (Suc One))) UnitNil


data Liter

instance Convertable VolumeUnit Liter where
	factor _ = 0.001
	showunit _ _ = "L"


data Gallon

instance Convertable VolumeUnit Gallon where
	factor _ = 0.00454609
	showunit _ _ = "gallon"


data FluidOunce

instance Convertable VolumeUnit FluidOunce where
	factor _ = 0.0000284130625
	showunit _ _ = "fl oz"

--

mile :: (Fractional f) => Value f LengthDimension Mile
mile = one

inch :: (Fractional f) => Value f LengthDimension Inch
inch = one

yard :: (Fractional f) => Value f LengthDimension Yard
yard = one

foot :: (Fractional f) => Value f LengthDimension Foot
foot = one

joerimijl :: (Fractional f) => Value f LengthDimension Joerimijl
joerimijl = one

ångström, angstrom :: (Fractional f) => Value f LengthDimension Ångström
ångström = one
angstrom = one

nautical_mile :: (Fractional f) => Value f LengthDimension NauticalMile
nautical_mile = one

--

barn :: (Fractional f) => Value f AreaUnit Barn
barn = one

--

liter :: (Fractional f) => Value f VolumeUnit Liter
liter = one

gallon :: (Fractional f) => Value f VolumeUnit Gallon
gallon = one

fluid_ounce :: (Fractional f) => Value f VolumeUnit FluidOunce
fluid_ounce = one