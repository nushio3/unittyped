{-# LANGUAGE DataKinds #-}
module UnitTyped.SI.Show where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import qualified UnitTyped.NoPrelude as NP
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Derived

import Data.Ratio

format_start :: (Convertible' a b, Convertible' c d, MapEq a c 'True) => Value Rational c d -> Value Rational a b -> (String, Value Rational c d)
format_start u v = format u ("", (NP.abs v) `as` u)

format :: (Convertible' a b, Convertible' a d) => Value Rational a d -> (String, Value Rational a b) -> (String, Value Rational a d)
format u (s, v) = let
					  fl_v = NP.floor (coerce v u)
					  str = (show $ numerator $ val $ fl_v) ++ " " ++ (showunit' $ proxy' u)
			      in (s ++ (if val fl_v == 0 then "" else (if not $ null s then ", " else "") ++ str), NP.diff $ coerce v u)

format_end :: (Convertible' a b, Convertible' a d) => Value Rational a d -> (String, Value Rational a b) -> String
format_end u (s, v) = (s ++ (if val v == 0 then "" else ((if not $ null s then ", " else "") ++ (show $ fromRational $ val (coerce v u)) ++ " " ++ (showunit' $ proxy' u))))

length_str :: (Convertible' a b, MapEq a LengthDimension 'True) => Value Rational a b -> String
length_str = meta_str meter

time_str :: (Convertible' a b, MapEq a TimeDimension 'True) => Value Rational a b -> String
time_str = format_end (mili second) . format second . format minute . format hour . format day . format_start year

meta_str :: (Convertible' a b, Convertible c d, MapEq a c 'True) => Value Rational c '[ '(d, POne) ] -> Value Rational a b -> String
meta_str unit v = format_end (yocto unit) $ format (zepto unit) $ format (atto unit) $ format (femto unit)
					$ format (pico unit) $ format (nano unit) $ format (micro unit) $ format (mili unit)
					$ format unit $ format (kilo unit) $ format (mega unit) $ format (giga unit)
					$ format (tera unit) $ format (peta unit) $ format (exa unit) $ format (zetta unit)
					$ format_start (yotta unit) (v `as` unit)