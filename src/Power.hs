{- | Defines a power algorithm to calculate powers based on an associative
     binary operation effectively. -}

module Power (
      power,
      powerM
      ) where

import Control.Monad.Identity

import Test.QuickCheck

-- COMMENT OUTDATED: Arguments are now power f n x
{- | Efficient power algorithm.

     > power x f n = x `f` x `f` x `f` ... (n times x)

     Examples:

     >>> power 2 (+) 3
     6
     >>> power 2 (*) 3
     8
     >>> power 2 (^) 3
     16

     The algorithm works by using the associativity of @f@, calculating each
     term only once. For example @2*2*2*2@ can be calculated with two
     multiplications by first calculating @2*2 = 4@, and then
     @2*2*2*2 = (2*2)*(2*2) = 4*4 = 16@.

     The calculation is tail recursive, and scales like log_2(n); for example
     @n = 10^10@ requires @43@ applications of @f@. I haven't tested the
     standard deviation on this, but the graph looked like @&#xb1;5@ in the
     @10^10@ region.

     The inner working is as follows:
     First, g is called. For @x <= 1@ it is the identity to work around
     the negative exponent issue; for even n, it uses @x^(2n) == (x^2)^n@
     and recurses.
     For uneven exponents, it gives way to @h@, which is essentially the same
     as @g@, but has another parameter to store the /rest value/ @r@:
     @x^(2n+1) == (x^2)^n * x@. It then recurses, collecting all the rests
     it encounters due to uneven exponents in the @r@ value.
     Note that this is implemented using 'powerM' and the 'Identity' monad.
     -}
power :: (Integral i) => (a -> a -> a) -> i -> a -> a
power f n = runIdentity . powerM (\a b -> Identity (f a b)) n
{-# INLINE power #-}

{- | Monadic version of 'power'.

     This one was used to determine the number of applications of @f@ by
     using the Sum monoid with the Writer monad, setting
     @f a b = writer (a `f` b, Sum 1)@. The number of applications of @f@ is
     then @getSum . snd . runWriter $ powerM 1 f n@.
     -}
powerM :: (Integral i, Monad m) => (a -> a -> m a) -> i -> a -> m a
powerM f = g
      where g n x
                  | n <= 0    = error "Negative exponent" -- Same as 1^(-1)
                  | n == 1    = return x
                  | even n    = x `f` x >>= g (n `quot` 2)
                  | otherwise = x `f` x >>= h (pred n `quot` 2) x
            h n r x
                  | n == 1    = x `f` r
                  | even n    = x `f` x >>= h (n `quot` 2) r
                  | otherwise = do square <- x `f` x
                                   rest   <- r `f` x
                                   h (pred n `quot` 2) rest square
{-# INLINE powerM #-}



-- QuickCheck part

main :: IO ()
main = runAll

runAll :: IO ()
runAll = do
      let args = stdArgs { maxSuccess = 10000 }
      quickCheckWith args prop_power
      quickCheckWith args prop_mult
      quickCheckWith args prop_concat

prop_power :: Property
prop_power = forAll arbitrary $ \(x, Positive n) ->
      classify (x == (0 :: Integer)) "base 0" $
      classify (x <  0) "negative base" $
      classify (n > 10) "large exponent" $
      (x :: Integer)^(n :: Integer) == power (*) n x

prop_mult :: Property
prop_mult = forAll arbitrary $ \(x, Positive n) ->
      classify (x == 0) "n times 0" $
      classify (x <  0) "negative factor" $
      classify (n > 10 || abs x > 10) "large factor" $
       (x :: Integer)*(n :: Integer) == power (+) n x

prop_concat :: Property
prop_concat = forAll arbitrary $ \(s, Positive n) ->
      classify (null s) "empty list" $
      power (++) (n :: Integer) (s :: [Int]) == concat (replicate (fromIntegral n) s)