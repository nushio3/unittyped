{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
-- |Module definig all SI units and their dimensions.
module UnitTyped.SI where

import Data.Typeable
import UnitTyped

-- |U for moles. Officially, this is a SI unit, but we definite it in terms of 'Count'.
data Mole
	deriving Typeable

instance Convertible Mole where
	factor _ = 6.0221417930e23
	showunit _ = "mole"
        type DimensionOf Mole = '[]
----
-- Length
----

-- |Dimension of length.
data Length
	deriving Typeable
-- |The dimension defining @Length^1@.
type LengthDimension = '[ '(Length, (Pos One)) ]

--

-- |The SI unit for 'Length': meter (m).
data Meter
	deriving Typeable

instance Convertible Meter where
	factor _ = 1
	showunit _ = "m"
        type DimensionOf Meter = LengthDimension
----
-- Time
----

-- |Dimension of time.
data Time
	deriving Typeable
-- |The dimension defining @Time^1@.
type TimeDimension = '[ '(Time, (Pos One)) ]

--

-- |The SI unit for 'Time': second (s).
data Second
	deriving Typeable

instance Convertible Second where
	factor _ = 1
	showunit _ = "s"
        type DimensionOf Second = TimeDimension
----
-- Mass
----

-- |Dimension of mass.
data Mass
	deriving Typeable
-- |The dimension defining @Mass^1@.
type MassDimension = '[ '(Mass, (Pos One)) ]

--

-- |The SI unit for 'Mass' is officially kilogram, but we define grams (g) here, so @kilo gram@ will work when using 'UnitTyped.SI.Meta'.
data Gram
	deriving Typeable

instance Convertible Gram where
	factor _ = 0.001
	showunit _ = "g"
        type DimensionOf Gram = MassDimension
----
-- Temperature
----

-- |Dimension of temperature.
data Temperature
	deriving Typeable
-- |The dimension defining @Temperature^1@.
type TemperatureDimension = '[ '(Temperature, (Pos One)) ]

--

-- |The SI unit for 'Temperature': Kelvin (K).
data Kelvin
	deriving Typeable

instance Convertible Kelvin where
	factor _ = 1
	showunit _ = "K"
        type DimensionOf Kelvin = TemperatureDimension
----
-- Current
----

-- |Dimension of electric current.
data Current
	deriving Typeable
-- |The dimension defining @Current^1@.
type CurrentDimension = '[ '(Current, (Pos One)) ]

--

-- |The SI unit for 'Current': ampere (A).
data Ampere
	deriving Typeable

instance Convertible Ampere where
	factor _ = 1
	showunit _ = "A"
        type DimensionOf Ampere = CurrentDimension 
----
-- Luminous
----

-- |Dimension of luminous intensity.
data Luminous
	deriving Typeable
-- |The dimension defining @Luminous^1@.
type LuminousDimension = '[ '(Luminous, (Pos One)) ]

--

-- |The SI unit for 'Luminous' intensity: candela (cd).
data Candela
	deriving Typeable

instance Convertible Candela where
	factor _ = 1
	showunit _ = "cd"
        type DimensionOf Candela = LuminousDimension
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
