{-# LANGUAGE BangPatterns #-}

module CommonFunctions where



import Data.List
import Control.Monad
import Control.Applicative
import Data.Char (digitToInt)
import Data.Tuple (swap)


-- | Integer square root
--   TODO?: Do trial re-multiplication to get rid of intermediate double
--   rounding errors
iSqrt :: (Integral a) => a -> a
iSqrt = floor . sqrt . fromIntegral
{-# INLINE iSqrt #-}

-- | Divisibility test.
--   Defined in terms of `rem` for speed, so be careful with negative numbers.
divisibleBy :: Integral a => a -> a -> Bool
a `divisibleBy` b = a `rem` b == 0
{-# INLINE divisibleBy #-}

-- | List of all divisors, i.e. proper divisors plus the number itself
divisors :: Integral a => a -> [a]
divisors n = map head . group . sort $ smaller ++ greater
      where smaller = filter (n `divisibleBy`) [1..iSqrt n]
            greater = map (n `quot`) smaller
{-# INLINE divisors #-}

-- | List of proper divisors of n
properDivisors :: Integral a => a -> [a]
properDivisors n | n == 1 = [1]
                 | otherwise = init . divisors $ n
{-# INLINE properDivisors #-}


digitSum :: Integral a => a -> a -> a
digitSum base = sum' . reverseExplodeInt base
{-# INLINE digitSum #-}

-- explodeInt with the result reversed. This is the natural outcome of the
-- unfoldr, and it is useful if the order of the digits obtained doens't matter,
-- e.g. for calculating the digit sum.
reverseExplodeInt :: Integral a => a -> a -> [a]
reverseExplodeInt base = unfoldr lastDigitof
      where lastDigitof n = swap (n `quotRem` base) <$ guard (n > 0)
{-# INLINE reverseExplodeInt #-}

-- Explodes an int to a list of digits.
-- explodeInt 10 1234 -> [4,3,2,1]
explodeInt :: Integral a => a -> a -> [a]
explodeInt base = reverse . reverseExplodeInt base
{-# INLINE explodeInt #-}

-- Specialized version of explodeInt, explodes with base 10. About a factor of
-- 10 faster than the unspecialized version (how appropriate).
explodeInt10 :: (Integral a, Show a) => a -> [a]
explodeInt10 = map (fromIntegral . digitToInt) . show


-- Inverse of explodeInt.
-- implodeInt 10 [1,2,3,4] -> 1234
implodeInt :: (Integral a) => a -> [a] -> a
implodeInt base = foldl' (\acc x -> base*acc + x) 0
{-# INLINE implodeInt #-}


-- Fibonacci numbers
fibo :: [Integer]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)
{-# NOINLINE fibo #-}

-- | Faculty function
faculty :: (Enum a, Num a) => a -> a
faculty n = foldl' (*) 1 [2..n] -- Using foldl1 crashes for n=0
{-# INLINE faculty #-}


-- | Binomial coefficient. Error for n or k < 0, and for n < k.
choose :: (Integral a) => a -> a -> a
choose n !k
      | n < k     = error "choose n k with n < k"
      | n < 0     = error "choose n k with n < 0"
      | k < 0     = error "choose n k with k < 0"
      | k == 0    = 1
      | n == 0    = 0
      | n - k < k = choose n (n - k)
      | otherwise = (n - k + 1) * choose n (k-1) `quot` k
{-# INLINE choose #-}
-- Here's a tail recursive version of the above algorithm, that is - to my
-- surprise - slower than the version above. I guess the thunk built up
-- above isn't so bad after all (well, choose is only used up to n,k <= 100 here
-- anyway, so stack overflows shouldn't matter now that I think of it)
--
-- choose n k = choose n k (1, 1)
--       where choose n k (num, denom)
--                   | n < k  = -1
--                   | n < 0  = -1
--                   | k < 0  = -1
--                   | k == 0 = num `quot` denom
--                   | n == 0 = 0
--                   | n - k < k = choose n (n-k) (num, denom)
--             choose n k (!num, !denom) = choose n (k-1) (num * (n-k+1), denom * k)


maximum' :: (Integral a) => [a] -> a
maximum' = foldl1' max
{-# INLINE maximum' #-}

-- | Strict product
product' :: Num a => [a] -> a
product' = foldl' (*) 1
{-# INLINE product' #-}

-- | Strict sum
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}