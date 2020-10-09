module Data.IntegerAdditive where

-- | A torsor over integer addition.
-- Instances satisfy:
--
-- * @iadd 0 = id@
--
-- * @iadd x . iadd y = iadd (x + y)@
--
-- * @idiff (iadd x a) a = x@
class Ord t => IntegerAdditive t where
    -- | iadd x a = x + a
    iadd :: Integer -> t -> t
    -- | idiff a b = a - b
    idiff :: t -> t -> Integer

instance IntegerAdditive Integer where
    iadd = (+)
    idiff = (-)
