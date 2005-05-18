{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.Private where

import Data.Fixed

show2 :: Int -> String
show2 i = let
	s = show i in
  case s of
	[_] -> '0':s
	_ -> s

show2Space :: Int -> String
show2Space i = let
	s = show i in
  case s of
	[_] -> ' ':s
	_ -> s

show2Fixed :: Pico -> String
show2Fixed x | x < 10 = '0':(showFixed True x)
show2Fixed x = showFixed True x

show3 :: Int -> String
show3 i = let
	s = show2 i in
  case s of
	[_,_] -> '0':s
	_ -> s

mod100 :: (Integral i) => i -> i
mod100 x = mod x 100

div100 :: (Integral i) => i -> i
div100 x = div x 100
