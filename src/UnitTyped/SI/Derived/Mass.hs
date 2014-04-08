{-# LANGUAGE DeriveDataTypeable #-}
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

-- |Units derived from the SI unit for mass.
module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

import Data.Typeable

----
-- Mass
----

-- |Pound, imperial unit of mass (lb).
data Pound
	deriving Typeable

instance Convertible Pound where
	factor _ = 0.45359237
	showunit _ = "lb"
        type DimensionOf Pound = MassDimension 
--

-- | Earth mass, a unit for mass of the Sun.
data EarthMass
	deriving Typeable

instance Convertible EarthMass where
	factor _ = 5.97219e24
	showunit _ = "M⊕"
        type DimensionOf EarthMass = MassDimension 
--

-- | Solar mass, a unit for mass of the Sun.
data SolarMass
	deriving Typeable

instance Convertible SolarMass where
	factor _ = 1.98855e30
	showunit _ = "M☉"
        type DimensionOf SolarMass = MassDimension 
--



-- |One pound (lb).
pound :: (Fractional f) => Value MassDimension (U Pound) f
pound = one


-- |One Earth mass.
earthMass :: (Fractional f) => Value MassDimension (U EarthMass) f
earthMass = one

-- |One Solar mass.
solarMass :: (Fractional f) => Value MassDimension (U SolarMass) f
solarMass = one