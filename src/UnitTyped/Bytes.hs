{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |A module for working with amounts of bytes and bits.
module UnitTyped.Bytes where

import UnitTyped
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived
import UnitTyped.SI

import Data.Typeable

----
-- Data
----

-- |An of amount of data.
data Data
	deriving Typeable
-- |The dimension representing @Data^1@.
type DataDimension = U Data

-- |A byte of data.
data Byte
	deriving Typeable

instance Convertible Byte where
	factor _ = 1
	showunit _ = "B"
        type DimensionOf Byte = DataDimension 

-- |A bit of data.
data Bit
	deriving Typeable

instance Convertible Bit where
	factor _ = 0.125
	showunit _ = "b"
        type DimensionOf Bit = DataDimension 
--

-- |One byte.
byte :: (Fractional f) => Value DataDimension (U Byte) f
byte = one

-- |One bit.
bit :: (Fractional f) => Value DataDimension (U Bit) f
bit = one