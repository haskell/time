{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Private where

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
