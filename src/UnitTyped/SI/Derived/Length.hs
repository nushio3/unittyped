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
type Miles = (Fractional f) => Value f LengthUnit Mile

instance Convertable LengthUnit Mile where
	factor _ = 1609.344
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

----
-- 3 dimensional
----

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

data FluidOunce

instance Convertable VolumeUnit FluidOunce where
	factor _ = 0.0000284130625
	showunit _ _ = "fl oz"

--

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

fluid_ounce :: (Fractional f) => Value f VolumeUnit FluidOunce
fluid_ounce = one