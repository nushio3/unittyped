module UnitTyped.Bytes where

import UnitTyped
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived
import UnitTyped.SI

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..))

----
-- Data
----

data Data
type DataUnit = UnitCons Data (Pos One) UnitNil


data Byte

instance Convertable DataUnit Byte where
	factor _ = 1
	showunit _ _ = "B"


data Bit

instance Convertable DataUnit Bit where
	factor _ = 0.125
	showunit _ _ = "b"

--

byte :: (Fractional f) => Value f DataUnit Byte
byte = one

bit :: (Fractional f) => Value f DataUnit Bit
bit = one