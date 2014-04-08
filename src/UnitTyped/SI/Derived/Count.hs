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

-- |A module with dimensionless units often used with SI.
module UnitTyped.SI.Derived.Count where

import UnitTyped
import UnitTyped.SI

import Data.Typeable

---
-- Count
---

-- |Percentage: 1% == 0.001
data Percentage
	deriving Typeable

instance Convertible Percentage where
	factor _ = 0.01
	showunit _ = "%"
        type DimensionOf Percentage = '[]

-- |Per mille: 1‰ == 0.001
data Permil
	deriving Typeable

instance Convertible Permil where
	factor _ = 0.001
	showunit _ = "‰"
        type DimensionOf Permil = '[]

-- |Parts per million: 1 ppm == 0.1^6
data Ppm
	deriving Typeable

instance Convertible Ppm where
	factor _ = 0.1^6
	showunit _ = "ppm"
        type DimensionOf Ppm = '[]

-- |Parts per billion: 1 ppb == 0.1^9
data Ppb
	deriving Typeable

instance Convertible Ppb where
	factor _ = 0.1^9
	showunit _ = "ppb"
        type DimensionOf Ppb = '[]

-- |Parts per trillion: 1 ppt == 0.1^12
data Ppt
	deriving Typeable

instance Convertible Ppt where
	factor _ = 0.1^12
	showunit _ = "ppt"
        type DimensionOf Ppt = '[]

-- |Angles are dimensionless, these are radians (rad).
data Radian
	deriving Typeable

instance Convertible Radian where
	factor _ = 1
	showunit _ = "rad"
        type DimensionOf Radian = '[]

-- |Angles are dimensionless, these are degrees (˚).
data Degree
	deriving Typeable

instance Convertible Degree where
	factor _ = 3.141592653589793 / 180
	showunit _ = "°"
        type DimensionOf Degree = '[]

--

-- |One percent (%).
percent :: (Fractional f) => Value '[] (U Percentage) f
percent = one

-- |One per mille (‰).
permil :: (Fractional f) => Value '[] (U Permil) f
permil = one

-- |One part per million (ppm).
ppm :: (Fractional f) => Value '[] (U Ppm) f
ppm = one

-- |One part per billion (ppb).
ppb :: (Fractional f) => Value '[] (U Ppb) f
ppb = one

-- |One part per trillion (ppt).
ppt :: (Fractional f) => Value '[] (U Ppt) f
ppt = one

-- |One rad (rad).
rad :: (Fractional f) => Value '[] (U Radian) f
rad = one

-- |One degree (˚).
deg :: (Fractional f) => Value '[] (U Degree) f
deg = one