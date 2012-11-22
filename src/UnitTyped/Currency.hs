{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module UnitTyped.Currency where

import UnitTyped.Units
import UnitTyped.SI.Meta

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

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

data Yen
type Yens = (Fractional f) => Value f CurrencyUnit Yen

instance Convertable CurrencyUnit Yen where
	factor _ = 0.009399
	showunit _ _ = "¥"

data Pound
type Pounds = (Fractional f) => Value f CurrencyUnit Pound

instance Convertable CurrencyUnit Pound where
	factor _ = 1.238
	showunit _ _ = "£"

data Gulden
type Guldens = (Fractional f) => Value f CurrencyUnit Gulden

instance Convertable CurrencyUnit Gulden where
	factor _ = 0.453780216
	showunit _ _ = "ƒ"

euro :: (Fractional f) => Value f CurrencyUnit Euro
euro = one

dollar :: (Fractional f) => Value f CurrencyUnit Dollar
dollar = one

yen :: (Fractional f) => Value f CurrencyUnit Yen
yen = one

pound :: (Fractional f) => Value f CurrencyUnit Pound
pound = one

gulden :: (Fractional f) => Value f CurrencyUnit Gulden
gulden = one
