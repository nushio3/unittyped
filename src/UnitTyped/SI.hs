module UnitTyped.SI where

import UnitTyped

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

--

data Count
type CountUnit = UnitNil

instance Convertable CountUnit Count where
	factor _ = 1
	showunit _ _ = "#"

--

data Mole
type MoleUnit = UnitCons Temperature (Pos One) UnitNil

instance Convertable CountUnit Mole where
	factor _ = 6.0221417930e23
	showunit _ _ = "mole"

----
-- Length
----

data Length
type LengthUnit = UnitCons Length (Pos One) UnitNil

--

data Meter
type Meters = (Fractional f) => Value f LengthUnit Meter

instance Convertable LengthUnit Meter where
	factor _ = 1
	showunit _ _ = "m"

----
-- Time
----

data Time
type TimeUnit = UnitCons Time (Pos One) UnitNil

--

data Second

type Seconds = (Fractional f) => Value f TimeUnit Second

instance Convertable TimeUnit Second where
	factor _ = 1
	showunit _ _ = "s"

----
-- Mass
----

data Mass
type MassUnit = UnitCons Mass (Pos One) UnitNil

data Gram
type Grams = (Fractional f) => Value f MassUnit Gram

instance Convertable MassUnit Gram where
	factor _ = 0.001
	showunit _ _ = "g"

----
-- Temperature
----

data Temperature
type TemperatureUnit = UnitCons Temperature (Pos One) UnitNil

data Kelvin
type Kelvins = (Fractional f) => Value f TemperatureUnit Kelvin

instance Convertable TemperatureUnit Kelvin where
	factor _ = 1
	showunit _ _ = "K"

----
-- Current
----

data Current
type CurrentUnit = UnitCons Current (Pos One) UnitNil

data Ampere
type Amperes = (Fractional f) => Value f CurrentUnit Ampere

instance Convertable CurrentUnit Ampere where
	factor _ = 1
	showunit _ _ = "A"

----
-- Luminous
----

data Luminous
type LuminousUnit = UnitCons Luminous (Pos One) UnitNil

data Candela
type Candelas = (Fractional f) => Value f LuminousUnit Candela

instance Convertable LuminousUnit Candela where
	factor _ = 1
	showunit _ _ = "cd"

----

count :: (Fractional f) => Value f CountUnit Count
count = one

mole :: (Fractional f) => Value f CountUnit Mole
mole = one

--

meter :: (Fractional f) => Value f LengthUnit Meter
meter = one

----

second :: (Fractional f) => Value f TimeUnit Second
second = one

--

gram :: (Fractional f) => Value f MassUnit Gram
gram = one

--

kelvin :: (Fractional f) => Value f TemperatureUnit Kelvin
kelvin = one

--

ampere :: (Fractional f) => Value f CurrentUnit Ampere
ampere = one

--

candela :: (Fractional f) => Value f LuminousUnit Candela
candela = one