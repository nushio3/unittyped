module UnitTyped.SI.Constants where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived
import UnitTyped.NoPrelude

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

-- π
pi :: (Fractional f, Prelude.Floating f) => Value f CountUnit Count
pi = mkVal Prelude.pi

-- speed of light
c :: (Fractional f) => Value f Speed (Div Meter Second)
c = mkVal 299792458

-- planck constant
h :: (Fractional f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
h = mkVal 6.6260695729e-34

-- atomic unit of action
hbar :: (Fractional f, Prelude.Floating f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
hbar = coerce (h / (2 . pi))

-- atomic unit of charge (elementary charge)
e :: (Fractional f) => Value f Charge Coulomb
e = mkVal 1.6021765314e-19

-- atomic unit of mass (electron mass)
m_e :: (Fractional f) => Value f MassUnit (Kilo Gram)
m_e = mkVal 9.109382616e-31

-- atomic unit of length
a_0 :: (Fractional f) => Value f LengthUnit Meter
a_0 = mkVal 0.529177210818e-10

-- atomic unit of energy
e_h :: (Fractional f) => Value f Energy Joule
e_h = mkVal 4.3597441775e-18

r :: (Fractional f) => Value f (UnitCons Temperature (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) (UnitCons Time (Neg (Suc One)) UnitNil)))) (Div Joule (Mul Kelvin Mole))
r = mkVal 8.314462175