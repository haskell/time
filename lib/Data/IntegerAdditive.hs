module Data.IntegerAdditive where

-- | A torsor over integer addition, used for a number of calendar types.
-- Instances satisfy:
--
-- * @iadd 0 = id@
--
-- * @iadd (x + y) = iadd x . iadd y@
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
