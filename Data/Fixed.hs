{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

module Data.Fixed
(
	fromReal,div',mod',divMod',

	Fixed,HasResolution(..),
	showFixed,
	E6,Micro,
	E12,Pico
) where

-- | similar idea to "fromIntegral"
fromReal :: (Real a,Fractional b) => a -> b
fromReal = fromRational . toRational

-- | like "div", but with a more useful type
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- | like "divMod", but with a more useful type
divMod' :: (Real a,Integral b) => a -> a -> (b,a)
divMod' n d = (f,n - (fromIntegral f) * d) where
	f = div' n d

-- | like "mod", but with a more useful type
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
	f = div' n d

newtype Fixed a = MkFixed Integer deriving (Eq,Ord,Enum)

class HasResolution a where
	resolution :: a -> Integer

fixedResolution :: (HasResolution a) => Fixed a -> Integer
fixedResolution fa = resolution (uf fa) where
	uf :: Fixed a -> a
	uf _ = undefined

withType :: (a -> f a) -> f a
withType foo = foo undefined

withResolution :: (HasResolution a) => (Integer -> f a) -> f a
withResolution foo = withType (foo . resolution)

instance (HasResolution a) => Num (Fixed a) where
	(MkFixed a) + (MkFixed b) = MkFixed (a + b)
	(MkFixed a) - (MkFixed b) = MkFixed (a - b)
	fa@(MkFixed a) * (MkFixed b) = MkFixed (div (a * b) (fixedResolution fa))
	negate (MkFixed a) = MkFixed (negate a)
	abs (MkFixed a) = MkFixed (abs a)
	signum (MkFixed a) = fromInteger (signum a)
	fromInteger i = withResolution (\res -> MkFixed (i * res))

instance (HasResolution a) => Real (Fixed a) where
	toRational fa@(MkFixed a) = (toRational a) / (toRational (fixedResolution fa))

instance (HasResolution a) => Fractional (Fixed a) where
	fa@(MkFixed a) / (MkFixed b) = MkFixed (div (a * (fixedResolution fa)) b)
	recip fa@(MkFixed a) = MkFixed (div (res * res) a) where
		res = fixedResolution fa
	fromRational r = withResolution (\res -> MkFixed (floor (r * (toRational res))))

instance (HasResolution a) => RealFrac (Fixed a) where
	properFraction a = (i,a - (fromIntegral i)) where
		i = truncate a
	truncate f = truncate (toRational f)
	round f = round (toRational f)
	ceiling f = ceiling (toRational f)
	floor f = floor (toRational f)

chopZeros :: Integer -> String
chopZeros 0 = ""
chopZeros a | mod a 10 == 0 = chopZeros (div a 10)
chopZeros a = show a

-- only works for positive a
showIntegerZeros :: Bool -> Int -> Integer -> String
showIntegerZeros True _ 0 = ""
showIntegerZeros chopTrailingZeros digits a = replicate (digits - length s) '0' ++ s' where
	s = show a
	s' = if chopTrailingZeros then chopZeros a else s

withDot :: String -> String
withDot "" = ""
withDot s = '.':s

-- | First arg is whether to chop off trailing zeros
showFixed :: (HasResolution a) => Bool -> Fixed a -> String
showFixed chopTrailingZeros fa@(MkFixed a) | a < 0 = "-" ++ (showFixed chopTrailingZeros (asTypeOf (MkFixed (negate a)) fa))
showFixed chopTrailingZeros fa@(MkFixed a) = (show i) ++ (withDot (showIntegerZeros chopTrailingZeros digits fracNum)) where
	res = fixedResolution fa
	(i,d) = divMod a res
	-- enough digits to be unambiguous
	digits = ceiling (logBase 10 (fromInteger res) :: Double)
	maxnum = 10 ^ digits
	fracNum = div (d * maxnum) res

instance (HasResolution a) => Show (Fixed a) where
	show = showFixed False



data E6 = E6

instance HasResolution E6 where
	resolution _ = 1000000

type Micro = Fixed E6


data E12 = E12

instance HasResolution E12 where
	resolution _ = 1000000000000

type Pico = Fixed E12
