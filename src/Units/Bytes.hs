{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.Bytes where

import Units.Units
import Units.SI.Meta
import Units.SI.Derived
import Units.SI

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

----
-- Data
----

data Data
type DataUnit = UnitCons Data (Pos One) UnitNil

data Byte
type Bytes = (Fractional f) => Value f DataUnit Byte

instance Convertable DataUnit Byte where
	factor _ = 1
	showunit _ _ = "B"

--

byte :: (Fractional f) => f -> Value f DataUnit Byte
byte = mkVal

kibibyte :: (Fractional f) => f -> Value f DataUnit (Kibi Byte)
kibibyte = mkVal
