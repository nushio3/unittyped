{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI.Constants where

import Units.Units
import Units.SI
import Units.SI.Meta
import Units.SI.Derived

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))


pi :: (Fractional f, Prelude.Floating f) => Value f CountUnit Count
pi = mkVal (Prelude.pi)

c :: (Fractional f) => Value f Speed (Div Meter Second)
c = mkVal 299792458

mole :: (Fractional f) => f -> Value f CountUnit Count
mole x = mkVal ((Prelude.*) 6.0221417930e23 x)

h :: (Fractional f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
h = mkVal 6.6260695729e-34

hbar :: (Fractional f, Prelude.Floating f) => Value f (UnitCons Time (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))) (Mul Joule Second)
hbar = coerce (h / (count 2 * pi))