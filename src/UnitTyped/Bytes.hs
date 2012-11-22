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
type Bytes = (Fractional f) => Value f DataUnit Byte

instance Convertable DataUnit Byte where
	factor _ = 1
	showunit _ _ = "B"

--

byte :: (Fractional f) => Value f DataUnit Byte
byte = one

kibibyte :: (Fractional f) => Value f DataUnit (Kibi Byte)
kibibyte = one
