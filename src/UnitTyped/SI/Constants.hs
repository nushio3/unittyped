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

-- |A number of mathematical or physical constants.
module UnitTyped.SI.Constants where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived

-- |π as the floating point value it has in the "Prelude".
pi' :: (Fractional f, Floating f) => Value f '[] '[]
pi' = mkVal Prelude.pi

-- |π as a rational value. Which it isn't. But we can pretend it is.
pi :: (Fractional f) => Value f '[] '[]
pi = mkVal 3.1415926535897932384626433832795028841971

-- |The speed of light
c :: (Fractional f) => Value f Speed '[ '(Second, NOne), '(Meter, POne)]
c = mkVal 299792458

-- |Planck constant
h :: Fractional f => Value f '[ '(Time, NOne), '(Mass, POne), '(Length, PTwo) ] '[ '(Joule, POne), '(Second, POne) ]
h = mkVal 6.6260695729e-34

-- |Reduced Planck constant
hbar :: Fractional f => Value f '[ '(Time, NOne), '(Length, PTwo), '(Mass, POne)] '[ '(Second, POne), '(Joule, POne) ]
hbar = coerce (h |/| (2 *| UnitTyped.SI.Constants.pi)) (error "hbar")

-- |Atomic unit of charge (elementary charge)
e :: (Fractional f) => Value f Charge (Unit Coulomb)
e = mkVal 1.6021765314e-19

-- |Atomic unit of mass (electron mass)
m_e :: (Fractional f) => Value f MassDimension (Unit (Kilo Gram))
m_e = mkVal 9.109382616e-31

-- |Atomic unit of length
a_0 :: (Fractional f) => Value f LengthDimension (Unit Meter)
a_0 = mkVal 0.529177210818e-10

-- |Atomic unit of energy
e_h :: (Fractional f) => Value f Energy (Unit Joule)
e_h = mkVal 4.3597441775e-18

-- |Gas constant.
r :: (Fractional f) => Value f '[ '(Temperature, NOne), '(Length, PTwo), '(Mass, POne), '(Time, NTwo) ] '[ '(Mole, NOne), '(Kelvin, NOne), '(Joule, POne) ]
r = mkVal 8.314462175

-- |Gravitational constant
g :: (Fractional f) => Value f '[ '(Time, NTwo), '(Length, PThree), '(Mass, NOne) ] '[ '(Second, NTwo), '(Meter, PThree), '((Kilo Gram), NOne) ]
g = mkVal 6.6738480e-11

---- |Planck mass
--m_P :: (Fractional f, Floating f) => Value f MassDimension (Unit (Kilo Gram))
--m_P = mkVal (sqrt (val $ hbar |*| c |/| g))

---- |Reduced Planck mass
--m_P' :: (Fractional f, Floating f) => Value f MassDimension (Unit (Kilo Gram))
--m_P' = mkVal (sqrt (val $ hbar |*| c |/| ((Prelude.pi * 8) *| g)))