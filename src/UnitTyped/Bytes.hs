{-# LANGUAGE DataKinds #-}
-- |A module for working with amounts of bytes and bits.
module UnitTyped.Bytes where

import UnitTyped
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived
import UnitTyped.SI

----
-- Data
----

-- |An of amount of data.
data Data
-- |The dimension representing @Data^1@.
type DataUnit = UnitCons Data (Pos One) UnitNil

-- |A byte of data.
data Byte

instance Convertable DataUnit Byte where
	factor _ = 1
	showunit _ _ = "B"

-- |A bit of data.
data Bit

instance Convertable DataUnit Bit where
	factor _ = 0.125
	showunit _ _ = "b"

--

-- |One byte.
byte :: (Fractional f) => Value f DataUnit Byte
byte = one

-- |One bit.
bit :: (Fractional f) => Value f DataUnit Bit
bit = one