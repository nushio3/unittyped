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
type DataDimension = '[ '(Data, POne) ]

-- |A byte of data.
data Byte

instance Convertible DataDimension Byte where
	factor _ = 1
	showunit _ = "B"

-- |A bit of data.
data Bit

instance Convertible DataDimension Bit where
	factor _ = 0.125
	showunit _ = "b"

--

-- |One byte.
byte :: (Fractional f) => Value DataDimension (Unit Byte) f
byte = one

-- |One bit.
bit :: (Fractional f) => Value DataDimension (Unit Bit) f
bit = one