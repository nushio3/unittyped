{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

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

instance Convertible LengthDimension Mile where
	factor _ = 1609.344
	showunit _ = "mile"

-- |The international inch (in).
data Inch

instance Convertible LengthDimension Inch where
	factor _ = 0.0254
	showunit _ = "in"

-- |The international yard (yd).
data Yard

instance Convertible LengthDimension Yard where
	factor _ = 0.9144
	showunit _ = "yd"

-- |The international foot (ft).
data Foot

instance Convertible LengthDimension Foot where
	factor _ = 0.3048
	showunit _ = "ft"

-- |Ångström, length unit for atoms and molecules (Å).
data Ångström

instance Convertible LengthDimension Ångström where
	factor _ = 10e-10
	showunit _ = "Å"

-- |Nautical miles (M).
data NauticalMile

instance Convertible LengthDimension NauticalMile where
	factor _ = 1852
	showunit _ = "M"

----
-- 2 dimensional
----

-- |Area: @Length^2@.
type AreaUnit = '[ '(Length, PTwo) ]

-- |Area, often used in nuclear physics (b).
data Barn

instance Convertible AreaUnit Barn where
	factor _ = 1e-28
	showunit _ = "b"

----
-- 3 dimensional
----

-- |Volume: @Length^3@.
type VolumeUnit = '[ '(Length, PThree) ]

-- |Liter, unit of volume (L).
data Liter

instance Convertible VolumeUnit Liter where
	factor _ = 0.001
	showunit _ = "L"

-- |Gallon, unit of volume (gallon).
data Gallon

instance Convertible VolumeUnit Gallon where
	factor _ = 0.00454609
	showunit _ = "gallon"

-- |Fluid ounce, unit of volume (fl oz).
data FluidOunce

instance Convertible VolumeUnit FluidOunce where
	factor _ = 0.0000284130625
	showunit _ = "fl oz"

--

-- |One mile (mile).
mile :: (Fractional f) => Value LengthDimension (Unit Mile) f
mile = one

-- |One inch (in).
inch :: (Fractional f) => Value LengthDimension (Unit Inch) f
inch = one

-- |One yard (yd).
yard :: (Fractional f) => Value LengthDimension (Unit Yard) f
yard = one

-- |One foot (ft).
foot :: (Fractional f) => Value LengthDimension (Unit Foot) f
foot = one

ångström, angstrom :: (Fractional f) => Value LengthDimension (Unit Ångström) f
-- |One ångström (Å).
ångström = one
-- |One ångström, for those with bad UTF-8 support (Å).
angstrom = one

-- |One nautical mile (M).
nautical_mile :: (Fractional f) => Value LengthDimension (Unit NauticalMile) f
nautical_mile = one

--

-- |One barn (b).
barn :: (Fractional f) => Value AreaUnit (Unit Barn) f
barn = one

--

-- |One liter (L).
liter :: (Fractional f) => Value VolumeUnit (Unit Liter) f
liter = one

-- |One gallon (gallon).
gallon :: (Fractional f) => Value VolumeUnit (Unit Gallon) f
gallon = one

-- |One fluid ounce (fl oz).
fluid_ounce :: (Fractional f) => Value VolumeUnit (Unit FluidOunce) f
fluid_ounce = one