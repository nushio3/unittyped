module UnitTyped.SI.Constants where

import UnitTyped.Units
import UnitTyped.SI
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived
import UnitTyped.NoPrelude

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))


pi :: (Fractional f, Prelude.Floating f) => Value f CountUnit Count
pi = (Prelude.pi) . one

c :: (Fractional f) => Value f Speed (Div Meter Second)
c = mkVal 299792458

h :: (Fractional f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
h = mkVal 6.6260695729e-34

hbar :: (Fractional f, Prelude.Floating f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
hbar = coerce (h / (2 . pi))

r :: (Fractional f) => Value f (UnitCons Temperature (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) (UnitCons Time (Neg (Suc One)) UnitNil)))) (Div Joule (Mul Kelvin Mole))
r = mkVal 8.314462175