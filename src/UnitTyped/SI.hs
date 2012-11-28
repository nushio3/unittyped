{-# LANGUAGE DataKinds #-}
-- |Module definig all SI units and their dimensions.
module UnitTyped.SI where

import UnitTyped

-- |Unit for moles. Officially, this is a SI unit, but we definite it in terms of 'Count'.
data Mole

instance Convertible '[] Mole where
	factor _ = 6.0221417930e23
	showunit _ = "mole"

----
-- Length
----

-- |Dimension of length.
data Length
-- |The dimension defining @Length^1@.
type LengthDimension = '[ '(Length, (Pos One)) ]

--

-- |The SI unit for 'Length': meter (m).
data Meter

instance Convertible LengthDimension Meter where
	factor _ = 1
	showunit _ = "m"

----
-- Time
----

-- |Dimension of time.
data Time
-- |The dimension defining @Time^1@.
type TimeDimension = '[ '(Time, (Pos One)) ]

--

-- |The SI unit for 'Time': second (s).
data Second

instance Convertible TimeDimension Second where
	factor _ = 1
	showunit _ = "s"

----
-- Mass
----

-- |Dimension of mass.
data Mass
-- |The dimension defining @Mass^1@.
type MassDimension = '[ '(Mass, (Pos One)) ]

--

-- |The SI unit for 'Mass' is officially kilogram, but we define grams (g) here, so @kilo gram@ will work when using 'UnitTyped.SI.Meta'.
data Gram

instance Convertible MassDimension Gram where
	factor _ = 0.001
	showunit _ = "g"

----
-- Temperature
----

-- |Dimension of temperature.
data Temperature
-- |The dimension defining @Temperature^1@.
type TemperatureDimension = '[ '(Temperature, (Pos One)) ]

--

-- |The SI unit for 'Temperature': Kelvin (K).
data Kelvin

instance Convertible TemperatureDimension Kelvin where
	factor _ = 1
	showunit _ = "K"

----
-- Current
----

-- |Dimension of electric current.
data Current
-- |The dimension defining @Current^1@.
type CurrentDimension = '[ '(Current, (Pos One)) ]

--

-- |The SI unit for 'Current': ampere (A).
data Ampere

instance Convertible CurrentDimension Ampere where
	factor _ = 1
	showunit _ = "A"

----
-- Luminous
----

-- |Dimension of luminous intensity.
data Luminous
-- |The dimension defining @Luminous^1@.
type LuminousDimension = '[ '(Luminous, (Pos One)) ]

--

-- |The SI unit for 'Luminous' intensity: candela (cd).
data Candela

instance Convertible LuminousDimension Candela where
	factor _ = 1
	showunit _ = "cd"

----

-- |One thing (#).
count :: (Fractional f) => Value f '[] '[]
count = one

-- |One mole (mol).
mole :: (Fractional f) => Value f '[] '[]
mole = one

--

-- |One meter (m).
meter :: (Fractional f) => Value f LengthDimension (Unit Meter)
meter = one

--

-- |One second (s).
second :: (Fractional f) => Value f TimeDimension (Unit Second)
second = one

--

-- |One gram (g).
gram :: (Fractional f) => Value f MassDimension (Unit Gram)
gram = one

--

-- |One Kelvin (K).
kelvin :: (Fractional f) => Value f TemperatureDimension (Unit Kelvin)
kelvin = one

--

-- |One ampere (A).
ampere :: (Fractional f) => Value f CurrentDimension (Unit Ampere)
ampere = one

--

-- |One candela (cd).
candela :: (Fractional f) => Value f LuminousDimension (Unit Candela)
candela = one
