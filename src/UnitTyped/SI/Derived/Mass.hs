{-# LANGUAGE DataKinds #-}
-- |Units derived from the SI unit for mass.
module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

----
-- Mass
----

-- |Pound, imperial unit of mass (lb).
data Pound

instance Convertible MassDimension Pound where
	factor _ = 0.45359237
	showunit _ = "lb"

--

-- |One pound (lb).
pound :: (Fractional f) => Value f MassDimension '[ '(Pound, POne) ]
pound = one