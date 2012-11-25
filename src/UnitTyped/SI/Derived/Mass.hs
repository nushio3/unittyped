module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

----
-- Mass
----

data PlanckMass

instance Convertable MassDimension PlanckMass where
	factor _ = 2.176513e-8
	showunit _ _ = "m_P"


data Pound

instance Convertable MassDimension Pound where
	factor _ = 0.45359237
	showunit _ _ = "lb"

--

m_P :: (Fractional f) => Value f MassDimension PlanckMass
m_P = one

pound :: (Fractional f) => Value f MassDimension Pound
pound = one