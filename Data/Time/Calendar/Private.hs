{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.Private where

import Data.Fixed

show2 :: (Num t,Ord t,Show t) => t -> String
show2 i | i < 0 = '-':(show2 (negate i))
show2 i = let
	s = show i in
  case s of
	[_] -> '0':s
	_ -> s

show2Space :: (Num t,Ord t,Show t) => t -> String
show2Space i | i < 0 = '-':(show2Space (negate i))
show2Space i = let
	s = show i in
  case s of
	[_] -> ' ':s
	_ -> s

show2Fixed :: Pico -> String
show2Fixed x | x < 10 = '0':(showFixed True x)
show2Fixed x = showFixed True x

show3 :: (Num t,Ord t,Show t) => t -> String
show3 i | i < 0 = '-':(show3 (negate i))
show3 i = let
	s = show2 i in
  case s of
	[_,_] -> '0':s
	_ -> s

show4 :: (Num t,Ord t,Show t) => t -> String
show4 i | i < 0 = '-':(show4 (negate i))
show4 i = let
	s = show3 i in
  case s of
	[_,_,_] -> '0':s
	_ -> s

mod100 :: (Integral i) => i -> i
mod100 x = mod x 100

div100 :: (Integral i) => i -> i
div100 x = div x 100

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x
