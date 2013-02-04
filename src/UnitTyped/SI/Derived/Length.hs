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

-- |Units derived from the SI unit for length, and higher dimensions of length (area, volume).
module UnitTyped.SI.Derived.Length where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived
import UnitTyped.SI.Meta

import Data.Typeable

import Data.Ratio

----
-- Length
----

-- |The international mile (mile).
data Mile
	deriving Typeable

instance Convertible Mile where
	factor _ = 1609.344
	showunit _ = "mile"
        type DimensionOf Mile = LengthDimension

-- |The international inch (in).
data Inch
	deriving Typeable

instance Convertible Inch where
	factor _ = 0.0254
	showunit _ = "in"
        type DimensionOf Inch = LengthDimension

-- |The international yard (yd).
data Yard
	deriving Typeable

instance Convertible Yard where
	factor _ = 0.9144
	showunit _ = "yd"
        type DimensionOf Yard = LengthDimension

-- |The international foot (ft).
data Foot
	deriving Typeable

instance Convertible Foot where
	factor _ = 0.3048
	showunit _ = "ft"
        type DimensionOf Foot = LengthDimension

-- |Ångström, length unit for atoms and molecules (Å).
data Ångström
	deriving Typeable

instance Convertible Ångström where
	factor _ = 10e-10
	showunit _ = "Å"
        type DimensionOf Ångström = LengthDimension

-- |Nautical miles (M).
data NauticalMile
	deriving Typeable

instance Convertible NauticalMile where
	factor _ = 1852
	showunit _ = "M"
        type DimensionOf NauticalMile = LengthDimension

----
-- 2 dimensional
----

-- |Area: @Length^2@.
type AreaDimension = '[ '(Length, PTwo) ]

-- |Area, often used in nuclear physics (b).
data Barn
	deriving Typeable

instance Convertible Barn where
	factor _ = 1e-28
	showunit _ = "b"
        type DimensionOf Barn = AreaDimension
----
-- 3 dimensional
----

-- |Volume: @Length^3@.
type VolumeDimension = '[ '(Length, PThree) ]

-- |Liter, unit of volume (L).
data Liter
	deriving Typeable

instance Convertible Liter where
	factor _ = 0.001
	showunit _ = "L"
        type DimensionOf Liter = VolumeDimension


-- |Gallon, unit of volume (gallon).
data Gallon
	deriving Typeable

instance Convertible Gallon where
	factor _ = 0.00454609
	showunit _ = "gallon"
        type DimensionOf Gallon = VolumeDimension

-- |Fluid ounce, unit of volume (fl oz).
data FluidOunce
	deriving Typeable

instance Convertible FluidOunce where
	factor _ = 0.0000284130625
	showunit _ = "fl oz"
        type DimensionOf FluidOunce = VolumeDimension

--

-- |One mile (mile).
mile :: (Fractional f) => Value LengthDimension (U Mile) f
mile = one

-- |One inch (in).
inch :: (Fractional f) => Value LengthDimension (U Inch) f
inch = one

-- |One yard (yd).
yard :: (Fractional f) => Value LengthDimension (U Yard) f
yard = one

-- |One foot (ft).
foot :: (Fractional f) => Value LengthDimension (U Foot) f
foot = one

ångström, angstrom :: (Fractional f) => Value LengthDimension (U Ångström) f
-- |One ångström (Å).
ångström = one
-- |One ångström, for those with bad UTF-8 support (Å).
angstrom = one

-- |One nautical mile (M).
nautical_mile :: (Fractional f) => Value LengthDimension (U NauticalMile) f
nautical_mile = one

--

-- |One barn (b).
barn :: (Fractional f) => Value AreaDimension (U Barn) f
barn = one

--

-- |One liter (L).
liter :: (Fractional f) => Value VolumeDimension (U Liter) f
liter = one

-- |One gallon (gallon).
gallon :: (Fractional f) => Value VolumeDimension (U Gallon) f
gallon = one

-- |One fluid ounce (fl oz).
fluid_ounce :: (Fractional f) => Value VolumeDimension (U FluidOunce) f
fluid_ounce = one