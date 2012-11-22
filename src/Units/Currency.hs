{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.Currency where

import Units.Units
import Units.SI.Meta

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))

data Currency
type CurrencyUnit = UnitCons Currency (Pos One) UnitNil

data Euro
type Euros = (Fractional f) => Value f CurrencyUnit Euro

instance Convertable CurrencyUnit Euro where
	factor _ = 1
	showunit _ _ = "€"

data Dollar
type Dollars = (Fractional f) => Value f CurrencyUnit Dollar

instance Convertable CurrencyUnit Dollar where
	factor _ = 0.7760
	showunit _ _ = "$"


euro :: (Fractional f) => f -> Value f CurrencyUnit Euro
euro = mkVal

dollar :: (Fractional f) => f -> Value f CurrencyUnit Dollar
dollar = mkVal