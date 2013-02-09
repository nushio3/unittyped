-- | Module that re-exports the common interface.
module UnitTyped (
        Convertible(..), Convertible'(..), U(..), (:|),
        Value(..), ValueProxy, ValueProxy', proxy',

        Count,

        Nat(..), Number(..),
        MapMerge, MapEq, MapNeg, MapTimes, MapStrip,
        POne, PTwo, PThree, PFour, PFive, PSix,
        NOne, NTwo, NThree, NFour,

        coerce, as, to, one, mkVal, val, 
        (|*|), (|/|), per, (|+|), (|-|),
        (*|), (|*), (|/), (/|), (|+), (+|), (|-), (-|),
        (|==|), (|<=|), (|<|), (|>=|), (|>|),

        dimension, unit,

        square, cubic
) where


import UnitTyped.Type

