{-# LANGUAGE DataKinds #-}
-- |A number of mathematical or physical constants.
module UnitTyped.SI.Constants where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived

-- |π as the floating point value it has in the "Prelude".
pi' :: (Fractional f, Floating f) => Value f NoDimension Count
pi' = mkVal Prelude.pi

-- |π as a rational value. Which it isn't. But we can pretend it is.
pi :: (Fractional f) => Value f NoDimension Count
pi = mkVal 3.1415926535897932384626433832795028841971

-- |The speed of light
c :: (Fractional f) => Value f Speed (Div Meter Second)
c = mkVal 299792458

-- |Planck constant
h :: (Fractional f) => Value f (UnitCons Time NOne (UnitCons Length PTwo (UnitCons Mass POne UnitNil))) (Mul Joule Second)
h = mkVal 6.6260695729e-34

-- |Reduced Planck constant
hbar :: (Fractional f) => Value f (UnitCons Time NOne (UnitCons Length PTwo (UnitCons Mass POne UnitNil))) (Mul Joule Second)
hbar = coerce (h ./. (2 .$. UnitTyped.SI.Constants.pi)) (joule .*. second)

-- |Atomic unit of charge (elementary charge)
e :: (Fractional f) => Value f Charge Coulomb
e = mkVal 1.6021765314e-19

-- |Atomic unit of mass (electron mass)
m_e :: (Fractional f) => Value f MassDimension (Kilo Gram)
m_e = mkVal 9.109382616e-31

-- |Atomic unit of length
a_0 :: (Fractional f) => Value f LengthDimension Meter
a_0 = mkVal 0.529177210818e-10

-- |Atomic unit of energy
e_h :: (Fractional f) => Value f Energy Joule
e_h = mkVal 4.3597441775e-18

-- |Gas constant.
r :: (Fractional f) => Value f (UnitCons Temperature NOne (UnitCons Length PTwo (UnitCons Mass POne (UnitCons Time (Neg (Suc One)) UnitNil)))) (Div Joule (Mul Kelvin Mole))
r = mkVal 8.314462175

-- |Gravitational constant
g :: (Fractional f) => Value f (UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos (Suc (Suc One))) (UnitCons Mass (Neg One) UnitNil))) (Div (Div (Mul (Mul Meter Meter) Meter) (Kilo Gram)) (Mul Second Second))
g = mkVal 6.6738480e-11

-- |Planck mass
m_P :: (Fractional f, Floating f) => Value f MassDimension (Kilo Gram)
m_P = mkVal (sqrt (val $ hbar .*. c ./. g))

-- |Reduced Planck mass
m_P' :: (Fractional f, Floating f) => Value f MassDimension (Kilo Gram)
m_P' = mkVal (sqrt (val $ hbar .*. c ./. ((Prelude.pi * 8) .$. g)))