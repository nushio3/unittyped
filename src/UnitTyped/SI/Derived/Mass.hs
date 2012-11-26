-- |Units derived from the SI unit for mass.
module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

----
-- Mass
----

-- |Pound, imperial unit of mass (lb).
data Pound

instance Convertable MassDimension Pound where
	factor _ = 0.45359237
	showunit _ _ = "lb"

--

-- |One pound (lb).
pound :: (Fractional f) => Value f MassDimension Pound
pound = one