module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

----
-- Mass
----

data PlanckMass

instance Convertable MassUnit PlanckMass where
	factor _ = 2.176513e-8
	showunit _ _ = "m_P"


data Pound

instance Convertable MassUnit Pound where
	factor _ = 0.45359237
	showunit _ _ = "lb"

--

m_P :: (Fractional f) => Value f MassUnit PlanckMass
m_P = one

pound :: (Fractional f) => Value f MassUnit Pound
pound = one