-- #hide
module Data.Time.Calendar.Private where

import Data.Fixed

type NumericPadOption = Maybe Char

pad1 :: NumericPadOption -> String -> String
pad1 (Just c) s = c:s
pad1 _ s = s

show2Fixed :: NumericPadOption -> Pico -> String
show2Fixed opt x | x < 10 = pad1 opt (showFixed True x)
show2Fixed _ x = showFixed True x

show2 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show2 opt i | i < 0 = '-':(show2 opt (negate i))
show2 opt i = let
	s = show i in
  case s of
	[_] -> pad1 opt s
	_ -> s

show3 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show3 opt i | i < 0 = '-':(show3 opt (negate i))
show3 opt i = let
	s = show2 opt i in
  case s of
	[_,_] -> pad1 opt s
	_ -> s

show4 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show4 opt i | i < 0 = '-':(show4 opt (negate i))
show4 opt i = let
	s = show3 opt i in
  case s of
	[_,_,_] -> pad1 opt s
	_ -> s

mod100 :: (Integral i) => i -> i
mod100 x = mod x 100

div100 :: (Integral i) => i -> i
div100 x = div x 100

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

clipValid :: (Ord t) => t -> t -> t -> Maybe t
clipValid a _ x | x < a = Nothing
clipValid _ b x | x > b = Nothing
clipValid _ _ x = Just x
