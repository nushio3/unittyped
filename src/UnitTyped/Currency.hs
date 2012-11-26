{-# LANGUAGE DataKinds #-}
-- |A module with a number of currencies. Values updated on: 22-11-2012
module UnitTyped.Currency where

import UnitTyped
import UnitTyped.SI.Meta

-- |Dimension of currency.
data Currency
-- |Dimension representing @Currency^1@.
type CurrencyUnit = UnitCons Currency (Pos One) UnitNil

-- |A euro (€).
data Euro

instance Convertible CurrencyUnit Euro where
	factor _ = 1
	showunit _ _ = "€"

-- |A dollar ($).
data Dollar

instance Convertible CurrencyUnit Dollar where
	factor _ = 0.7760
	showunit _ _ = "$"

-- |A Yen (¥).
data Yen

instance Convertible CurrencyUnit Yen where
	factor _ = 0.009399
	showunit _ _ = "¥"

-- |British Pound (£)
data Pound

instance Convertible CurrencyUnit Pound where
	factor _ = 1.238
	showunit _ _ = "£"

-- |No longer used, Dutch Guilders (ƒ)
data Gulden

instance Convertible CurrencyUnit Gulden where
	factor _ = 0.453780216
	showunit _ _ = "ƒ"

--

-- |One euro.
euro :: (Fractional f) => Value f CurrencyUnit Euro
euro = one

-- |One eurocent.
eurocent :: (Fractional f) => Value f CurrencyUnit Euro
eurocent = mkVal 0.01

-- |One dollar.
dollar :: (Fractional f) => Value f CurrencyUnit Dollar
dollar = one

-- |One dollarcent.
dollarcent :: (Fractional f) => Value f CurrencyUnit Dollar
dollarcent = mkVal 0.01

-- |One yen.
yen :: (Fractional f) => Value f CurrencyUnit Yen
yen = one

-- |One pound.
pound :: (Fractional f) => Value f CurrencyUnit Pound
pound = one

-- |One gulden.
gulden :: (Fractional f) => Value f CurrencyUnit Gulden
gulden = one
