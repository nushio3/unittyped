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

-- |Module that allows pretty-printing of a value into multiple units. For example:
--
-- >>> length_str (42 *| mile)
--  "67 km, 592 m, 448 mm"
module UnitTyped.SI.Show where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import qualified UnitTyped.NoPrelude as NP
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Derived

import Data.Ratio

-- |Start a chain of units. For example:
--
-- > format_end second $ format minute $ format_start hour x
format_start :: (Convertible' a b, Convertible' c d, MapEq a c) => Value Rational c d -> Value Rational a b -> (String, Value Rational c d)
format_start u v = format u ("", (NP.abs v) `as` u)

-- |Add a unit in between 'format_start' and 'format_end'.
format :: (Convertible' a b, Convertible' a d) => Value Rational a d -> (String, Value Rational a b) -> (String, Value Rational a d)
format u (s, v) = let
					  fl_v = NP.floor (coerce v u)
					  str = (show $ numerator $ val $ fl_v) ++ " " ++ (showunit' $ proxy' u)
			      in (s ++ (if val fl_v == 0 then "" else (if not $ null s then ", " else "") ++ str), NP.diff $ coerce v u)

-- |End the chain of formatters.
format_end :: (Convertible' a b, Convertible' a d) => Value Rational a d -> (String, Value Rational a b) -> String
format_end u (s, v) = (s ++ (if val v == 0 then "" else ((if not $ null s then ", " else "") ++ (show $ fromRational $ val (coerce v u)) ++ " " ++ (showunit' $ proxy' u))))

-- |Show a time range as years, days, hours, minutes, seconds and miliseconds.
time_str :: (Convertible' a b, MapEq a TimeDimension) => Value Rational a b -> String
time_str = format_end (mili second) . format second . format minute . format hour . format day . format_start year

-- |Show a unit with all possible SI-prefixes.
-- For instance, a light year in meters:
--
-- >>> meta_str meter (c |*| 1 *| year)
-- "9 Pm, 460 Tm, 536 Gm, 207 Mm, 68 km, 16 m"
meta_str :: (Convertible' a b, Convertible c d, MapEq a c, MapNull (MapMerge (MapTimes POne (MapNeg c)) c)) => Value Rational c (Unit d) -> Value Rational a b -> String
meta_str unit v = format_end (yocto unit) $ format (zepto unit) $ format (atto unit) $ format (femto unit)
					$ format (pico unit) $ format (nano unit) $ format (micro unit) $ format (mili unit)
					$ format unit $ format (kilo unit) $ format (mega unit) $ format (giga unit)
					$ format (tera unit) $ format (peta unit) $ format (exa unit) $ format (zetta unit)
					$ format_start (yotta unit) (v `as` unit)