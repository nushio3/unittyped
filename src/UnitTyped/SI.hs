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
-- |Module definig all SI units and their dimensions.
module UnitTyped.SI where

import Data.Typeable
import UnitTyped

-- |U for moles. Officially, this is a SI unit, but we definite it in terms of 'Count'.
data Mole
	deriving Typeable

instance Convertible '[] Mole where
	factor _ = 6.0221417930e23
	showunit _ = "mole"

----
-- Length
----

-- |Dimension of length.
data Length
	deriving Typeable
-- |The dimension defining @Length^1@.
type LengthDimension = '[ '(Length, POne) ]

--

-- |The SI unit for 'Length': meter (m).
data Meter
	deriving Typeable

instance Convertible LengthDimension Meter where
	factor _ = 1
	showunit _ = "m"

----
-- Time
----

-- |Dimension of time.
data Time
	deriving Typeable
-- |The dimension defining @Time^1@.
type TimeDimension = '[ '(Time, POne) ]

--

-- |The SI unit for 'Time': second (s).
data Second
	deriving Typeable

instance Convertible TimeDimension Second where
	factor _ = 1
	showunit _ = "s"

----
-- Mass
----

-- |Dimension of mass.
data Mass
	deriving Typeable
-- |The dimension defining @Mass^1@.
type MassDimension = '[ '(Mass, POne) ]

--

-- |The SI unit for 'Mass' is officially kilogram, but we define grams (g) here, so @kilo gram@ will work when using 'UnitTyped.SI.Meta'.
data Gram
	deriving Typeable

instance Convertible MassDimension Gram where
	factor _ = 0.001
	showunit _ = "g"

----
-- Temperature
----

-- |Dimension of temperature.
data Temperature
	deriving Typeable
-- |The dimension defining @Temperature^1@.
type TemperatureDimension = '[ '(Temperature, POne) ]

--

-- |The SI unit for 'Temperature': Kelvin (K).
data Kelvin
	deriving Typeable

instance Convertible TemperatureDimension Kelvin where
	factor _ = 1
	showunit _ = "K"

----
-- Current
----

-- |Dimension of electric current.
data Current
	deriving Typeable
-- |The dimension defining @Current^1@.
type CurrentDimension = '[ '(Current, POne) ]

--

-- |The SI unit for 'Current': ampere (A).
data Ampere
	deriving Typeable

instance Convertible CurrentDimension Ampere where
	factor _ = 1
	showunit _ = "A"

----
-- Luminous
----

-- |Dimension of luminous intensity.
data Luminous
	deriving Typeable
-- |The dimension defining @Luminous^1@.
type LuminousDimension = '[ '(Luminous, POne) ]

--

-- |The SI unit for 'Luminous' intensity: candela (cd).
data Candela
	deriving Typeable

instance Convertible LuminousDimension Candela where
	factor _ = 1
	showunit _ = "cd"

----

-- |One thing (#).
count :: (Fractional f) => Value '[] '[] f
count = one

-- |One mole (mol).
mole :: (Fractional f) => Value '[] '[] f
mole = one

--

-- |One meter (m).
meter :: (Fractional f) => Value LengthDimension (U Meter) f
meter = one

--

-- |One second (s).
second :: (Fractional f) => Value TimeDimension (U Second) f
second = one

--

-- |One gram (g).
gram :: (Fractional f) => Value MassDimension (U Gram) f
gram = one

--

-- |One Kelvin (K).
kelvin :: (Fractional f) => Value TemperatureDimension (U Kelvin) f
kelvin = one

--

-- |One ampere (A).
ampere :: (Fractional f) => Value CurrentDimension (U Ampere) f
ampere = one

--

-- |One candela (cd).
candela :: (Fractional f) => Value LuminousDimension (U Candela) f
candela = one
