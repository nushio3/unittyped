module UnitTyped.SI.Derived.Count where

import UnitTyped
import UnitTyped.SI

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