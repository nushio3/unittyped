-- |Units derived from the SI unit for length, and higher dimensions of length (area, volume).
module UnitTyped.SI.Derived.Length where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived
import UnitTyped.SI.Meta

import Data.Ratio

----
-- Length
----

-- |The international mile (mile).
data Mile

instance Convertable LengthDimension Mile where
	factor _ = 1609.344
	showunit _ _ = "mile"

-- |The international inch (in).
data Inch

instance Convertable LengthDimension Inch where
	factor _ = 0.0254
	showunit _ _ = "in"

-- |The international yard (yd).
data Yard

instance Convertable LengthDimension Yard where
	factor _ = 0.9144
	showunit _ _ = "yd"

-- |The international foot (ft).
data Foot

instance Convertable LengthDimension Foot where
	factor _ = 0.3048
	showunit _ _ = "ft"

-- |Ångström, length unit for atoms and molecules (Å).
data Ångström

instance Convertable LengthDimension Ångström where
	factor _ = 10e-10
	showunit _ _ = "Å"

-- |Nautical miles (M).
data NauticalMile

instance Convertable LengthDimension NauticalMile where
	factor _ = 1852
	showunit _ _ = "M"

----
-- 2 dimensional
----

-- |Area: @Length^2@.
type AreaUnit = UnitCons Length (Pos (Suc One)) UnitNil

-- |Area, often used in nuclear physics (b).
data Barn

instance Convertable AreaUnit Barn where
	factor _ = 1e-28
	showunit _ _ = "b"

----
-- 3 dimensional
----

-- |Volume: @Length^3@.
type VolumeUnit = UnitCons Length (Pos (Suc (Suc One))) UnitNil

-- |Liter, unit of volume (L).
data Liter

instance Convertable VolumeUnit Liter where
	factor _ = 0.001
	showunit _ _ = "L"

-- |Gallon, unit of volume (gallon).
data Gallon

instance Convertable VolumeUnit Gallon where
	factor _ = 0.00454609
	showunit _ _ = "gallon"

-- |Fluid ounce, unit of volume (fl oz).
data FluidOunce

instance Convertable VolumeUnit FluidOunce where
	factor _ = 0.0000284130625
	showunit _ _ = "fl oz"

--

-- |One mile (mile).
mile :: (Fractional f) => Value f LengthDimension Mile
mile = one

-- |One inch (in).
inch :: (Fractional f) => Value f LengthDimension Inch
inch = one

-- |One yard (yd).
yard :: (Fractional f) => Value f LengthDimension Yard
yard = one

-- |One foot (ft).
foot :: (Fractional f) => Value f LengthDimension Foot
foot = one

ångström, angstrom :: (Fractional f) => Value f LengthDimension Ångström
-- |One ångström (Å).
ångström = one
-- |One ångström, for those with bad UTF-8 support (Å).
angstrom = one

-- |One nautical mile (M).
nautical_mile :: (Fractional f) => Value f LengthDimension NauticalMile
nautical_mile = one

--

-- |One barn (b).
barn :: (Fractional f) => Value f AreaUnit Barn
barn = one

--

-- |One liter (L).
liter :: (Fractional f) => Value f VolumeUnit Liter
liter = one

-- |One gallon (gallon).
gallon :: (Fractional f) => Value f VolumeUnit Gallon
gallon = one

-- |One fluid ounce (fl oz).
fluid_ounce :: (Fractional f) => Value f VolumeUnit FluidOunce
fluid_ounce = one