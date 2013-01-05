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

-- |A module with dimensionless units often used with SI.
module UnitTyped.SI.Derived.Count where

import UnitTyped
import UnitTyped.SI

---
-- Count
---

-- |Percentage: 1% == 0.001
data Percentage

instance Convertible '[] Percentage where
	factor _ = 0.01
	showunit _ = "%"

-- |Per mille: 1‰ == 0.001
data Permil

instance Convertible '[] Permil where
	factor _ = 0.001
	showunit _ = "‰"

-- |Parts per million: 1 ppm == 0.1^6
data Ppm

instance Convertible '[] Ppm where
	factor _ = 0.1^6
	showunit _ = "ppm"

-- |Parts per billion: 1 ppb == 0.1^9
data Ppb

instance Convertible '[] Ppb where
	factor _ = 0.1^9
	showunit _ = "ppb"

-- |Parts per trillion: 1 ppt == 0.1^12
data Ppt

instance Convertible '[] Ppt where
	factor _ = 0.1^12
	showunit _ = "ppt"

-- |Angles are dimensionless, these are radians (rad).
data Radian

instance Convertible '[] Radian where
	factor _ = 1
	showunit _ = "rad"

-- |Angles are dimensionless, these are degrees (˚).
data Degree

instance Convertible '[] Degree where
	factor _ = 3.141592653589793 / 180
	showunit _ = "°"

--

-- |One percent (%).
percent :: (Fractional f) => Value '[] (Unit Percentage) f
percent = one

-- |One per mille (‰).
permil :: (Fractional f) => Value '[] (Unit Permil) f
permil = one

-- |One part per million (ppm).
ppm :: (Fractional f) => Value '[] (Unit Ppm) f
ppm = one

-- |One part per billion (ppb).
ppb :: (Fractional f) => Value '[] (Unit Ppb) f
ppb = one

-- |One part per trillion (ppt).
ppt :: (Fractional f) => Value '[] (Unit Ppt) f
ppt = one

-- |One rad (rad).
rad :: (Fractional f) => Value '[] (Unit Radian) f
rad = one

-- |One degree (˚).
deg :: (Fractional f) => Value '[] (Unit Degree) f
deg = one