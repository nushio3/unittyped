{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- |Units derived from the SI unit for mass.
module UnitTyped.SI.Derived.Mass where

import UnitTyped
import UnitTyped.SI

import Data.Typeable

----
-- Mass
----

-- |Pound, imperial unit of mass (lb).
data Pound
	deriving Typeable

instance Convertible MassDimension Pound where
	factor _ = 0.45359237
	showunit _ = "lb"

--

-- |One pound (lb).
pound :: (Fractional f) => Value MassDimension (U Pound) f
pound = one