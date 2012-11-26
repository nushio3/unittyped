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

instance Convertible NoDimension Percentage where
	factor _ = 0.01
	showunit _ _ = "%"

-- |Per mille: 1‰ == 0.001
data Permil

instance Convertible NoDimension Permil where
	factor _ = 0.001
	showunit _ _ = "‰"

-- |Parts per million: 1 ppm == 0.1^6
data Ppm

instance Convertible NoDimension Ppm where
	factor _ = 0.1^6
	showunit _ _ = "ppm"

-- |Parts per billion: 1 ppb == 0.1^9
data Ppb

instance Convertible NoDimension Ppb where
	factor _ = 0.1^9
	showunit _ _ = "ppb"

-- |Parts per trillion: 1 ppt == 0.1^12
data Ppt

instance Convertible NoDimension Ppt where
	factor _ = 0.1^12
	showunit _ _ = "ppt"

-- |Angles are dimensionless, these are radians (rad).
data Radian

instance Convertible NoDimension Radian where
	factor _ = 1
	showunit _ _ = "rad"

-- |Angles are dimensionless, these are degrees (˚).
data Degree

instance Convertible NoDimension Degree where
	factor _ = 3.141592653589793 / 180
	showunit _ _ = "°"

--

-- |One percent (%).
percent :: (Fractional f) => Value f NoDimension Percentage
percent = one

-- |One per mille (‰).
permil :: (Fractional f) => Value f NoDimension Permil
permil = one

-- |One part per million (ppm).
ppm :: (Fractional f) => Value f NoDimension Ppm
ppm = one

-- |One part per billion (ppb).
ppb :: (Fractional f) => Value f NoDimension Ppb
ppb = one

-- |One part per trillion (ppt).
ppt :: (Fractional f) => Value f NoDimension Ppt
ppt = one

-- |One rad (rad).
rad :: (Fractional f) => Value f NoDimension Radian
rad = one

-- |One degree (˚).
deg :: (Fractional f) => Value f NoDimension Degree
deg = one