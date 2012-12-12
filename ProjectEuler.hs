{-# LANGUAGE BangPatterns #-}

module ProjectEuler (
      euler
) where

import Debug.Trace
import Data.Ord
import Data.Tuple
import Data.List
import Data.Maybe
import Data.Char
import Data.Ratio
import Control.Applicative
import Control.Monad.State
import qualified Data.Numbers.Primes as Primes -- cabal install primes
import qualified Data.Permute as Permute       -- cabal install permutation
import qualified Data.Vector as Vec
import qualified Data.Array.IArray as A
import Control.DeepSeq
import Data.Bits
import qualified Power


import qualified Problem8  as P8
import qualified Problem11 as P11
import qualified Problem13 as P13
import qualified Problem18 as P18
import qualified Problem22 as P22
import qualified Problem42 as P42
import qualified Problem59 as P59
import qualified Problem67 as P67
import qualified Problem99 as P99

euler :: Int -> Maybe Integer

-- All performance is measured in GHCi, i.e. without optimization.

-- | Integer square root
iSqrt :: (Integral a) => a -> a
iSqrt = floor . sqrt . fromIntegral
{-# INLINE iSqrt #-}

-- | Divisibility test
divisibleBy :: Integral a => a -> a -> Bool
a `divisibleBy` b = a `mod` b == 0
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
{-# NOINLINE fibo #-} -- Probably unnecessary, GHC is quite smart and

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

-- | Strict sum
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0



{-
      Problem 1
            Find the sum of all the multiples of 3 or 5 below 1000.

      Result
            233168
            .01 s

      Comment
            The formula below is based on sum [1..n] == n(n+1)/2.
-}
euler 1 = Just $ (sumFor 3 + sumFor 5 - sumFor 15) `quot` 2
      where nMax = 1000 - 1
            sumFor n = let nX = nMax `quot` n
                       in  n * nX * (nX + 1)
-- Old implementation
-- euler 1 = Just $ return $ sum [0,3..999] + sum [0,5..999]
-- Even older implementation
-- euler 1 = Just $ return $ sum [n | n <- [1..999], n `divisibleBy` 5 || n `divisibleBy` 3]



{-
      Problem 2
            Find the sum of the even-valued Fibonacci numbers <= 4e6.

      Result
            .00 s
-}
euler 2 = Just $  sum' . filter even $ takeWhile (<= 4 * 10^6) fibo



{-
      Problem 3
            The prime factors of 13195 are 5, 7, 13 and 29.
            What is the largest prime factor of the number 600851475143?

      Result
            .01 s
-}
euler 3 = Just $ last . Primes.primeFactors $ 600851475143


{-
      Problem 4
            A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 ? 99.
            Find the largest palindrome made from the product of two 3-digit numbers.

      Result
            .67 s
-}
euler 4 = Just $ maximum' [i*j | i <- [1..999], j <- [1..i], isPalindromic  (i*j)]
      where isPalindromic n = let showNumber = show n
                              in  showNumber == reverse showNumber




{-
      Problem 5
            What is the smallest positive number that is evenly divisible by all
            of the numbers from 1 to 20?

      Result
            .00 s

      Comment
            Haskell provides a lcm function in the Prelude, but I re-implemented
            it so that the problem is less of a stub.
-}
euler 5 = Just $ foldl1' lcm' [1..20]
      where lcm' a b = a `quot` gcd' a b * b
            gcd' a 0 = a
            gcd' a b = gcd' b (a `rem` b)



{-
      Problem 6
            Find the difference between the sum of the squares of the first one
            hundred natural numbers and the square of the sum.

      Result
            25164150
            .00 s

      Alternative solution:
            The brute force formula, which when simplified yields the solution
            below, is
                  result = squareSum - sumSquares
                  squareSum  = ((^2) . sum) range
                  sumSquares = (sum . map (^2)) range
                  range = [1..100]
-}
euler 6 = Just $ (n-1) * n * (1+n) * (2+3*n) `div` 12
      where n = 100



{-
      Problem 7
            What is the 10001st prime number?

      Performance:
            104743
            .04 s
-}
euler 7 = Just $ Primes.primes !! (10001-1) -- Lists start counting at 0



{-
      Problem 8
            Find the greatest product of five consecutive digits in the 1000-digit number.

      Performance:
            .03 s
-}
euler 8 = Just $ largestProduct 5 . explodeInt10 $ P8.n
      where largestProduct n = maximum . map product' . transpose . take n . tails



{-
      Problem 9
            Find the product a*b*c of the only Pythagorean triplet (a, b, c) for
            which a + b + c = 1000.

      Result
            31875000
            .58 s
-}
euler 9 = listToMaybe $ do
      c <- [1..]
      b <- [1..c-1]
      let a = 1000 - b - c
      guard $ a^2 + b^2 == c^2
      return $ a*b*c



{-
      Problem 10
            Find the sum of all primes <= 2 * 10^6

      Result
            142913828922
            0.55 s
-}
euler 10 = Just . sum' . takeWhile (<= 2 * 10^6) $ Primes.primes



{-
      Problem 11
            Find the greatest product of adjacent numbers in a grid

      Result
            70600674
            0.02 s
-}
euler 11 = Just . maximum' . map maxProduct $ horizontal ++ vertical ++ diag1 ++ diag2
      where
            -- max product of 4 consecutive numbers in a list
            maxProduct = maximum' . map (product' . take 4) . tails
            rotateList _ [] = []
            rotateList n xs -- Example: rotateList (-1) [1..4] == [4,1,2,3]
                  | n == 0 = xs
                  | n >  0 = rotateList (n-1) $  tail xs  ++ [head xs]
                  | n <  0 = rotateList (n+1) $ [last xs] ++  init xs
            horizontal = P11.grid
            vertical   = transpose horizontal
            diag1      = transpose $ zipWith rotateList [1..]     horizontal
            diag2      = transpose $ zipWith rotateList [-1,-2..] horizontal


{-
      Problem 12
            What is the value of the first triangle number to have over five
            hundred divisors?

      Result
            76576500
            0.74 s
-}
euler 12 = listToMaybe [triangle n | n <- [1..], triangleD n > 500]
      where -- Calculates the n-th triangle number
            triangle n = n * (n+1) `quot` 2

            -- Number of divisors of the triangle number n*(n+1)/2. Uses the
            -- fact that n and (n+1) are coprime, so the divisor count is a
            -- homomorphism on them.
            triangleD n
                  | even n    = d (n `quot` 2) * d (n + 1) -- Make sure the even value ...
                  | otherwise = d n * d ((n + 1) `quot` 2) -- ... is divided by 2
            d = numDivisors

            -- Number of divisors.
            -- Based on the fact that a number with prime factors of multi-
            -- plicities a1, a2, ... has (a1+1)(a2+1)... divisors.
            -- Equivalent to length.divisors, but performs way better.
            numDivisors = product . map (+1) . pfm
            -- pfm = prime factor multiplicities
            pfm = map length . group . Primes.primeFactors



{-
      Problem 13
            Work out the first ten digits of the sum of one-hundred 50-digit
            numbers.

      Result
            5537376230
            .01 s
-}
euler 13 = Just . first10 . sum' $ P13.numbers
      where first10 n | n < 10^10 = n -- first number with 11 digits is 10^10
                      | otherwise  = first10 $ n `quot` 10


{-
      Problem 14
            Which starting number, under one million, produces the longest
            Collatz chain?

      Result
            837799
            10 s
-}
euler 14 = Just $ fst maxTuple
      where maxN :: Integer
            maxN = 10^6-1

            -- How many numbers to memoize.
            -- Too large consumes too much space, too little and memoization
            -- is useless.
            memoN :: Integer
            memoN = maxN

            -- Vector of the type [(n, collatz steps n has to take to reach loop)],
            -- intended to memoize already calculated lengths below a million.
            -- The first element is (1,1), since 1 has to take 1 step to loop.
            collatzLengthList :: A.Array Integer Integer
            collatzLengthList = A.array (1, memoN) [(n, collatzLength collatzLengthList n 1) | n <- [1..memoN]]
            -- collatzLengthList = V.generate (maxN+1) (\n -> if n == 0 then (0,-1) else (n, collatzLength collatzLengthList n 1))

            collatzLength :: A.Array Integer Integer -> Integer -> Integer -> Integer
            collatzLength memo n c
                  | n == 1                                           = 1
                  | even n && A.bounds memo `A.inRange` (n `quot` 2) = c + (memo A.! (n `quot` 2))
                  | odd  n && A.bounds memo `A.inRange`  (3 * n + 1) = c + (memo A.! (3 * n +  1))
                  | even n                                           = collatzLength memo (n `quot` 2) (c+1)
                  | otherwise                                        = collatzLength memo (3*n + 1) (c+1)

            maxTuple = foldl1 maxC . take (fromIntegral maxN) $ A.assocs collatzLengthList
                  where maxC a@(_,!a2) x@(_,x2)
                              | a2 >= x2  = a
                              | otherwise = x

-- Old algorithm, using no memoization. Performance: 228 s (GHCi), 1.3 s (GHC).
--
-- euler 14 = Just $ return . fromIntegral . snd . maxTuple $ [1..10^6-1]
--    where

--          collatzLength :: Word32 -> Word16
--          collatzLength n = go (1, n)
--                where
--                      -- 'go (1, n)' calculates the length of the collatz chain
--                      -- starting at n.
--                      go :: (Word16, Word32) -> Word16
--                      go (!c, !n) -- c = current count so far, n = current number
--                            | n == 1    = c
--                            | even n    = go (c + 1, n `div` 2)
--                            | otherwise = go (c + 1, 3 * n + 1)

--          -- 'collatzMax prev candid' returns the tuple with the longer chain,
--          -- which is either the unmodified old tuple, or the one generated
--          -- out of the starting value provided as the second argument.
--          collatzMax :: (Word16, Word32) -> Word32 -> (Word16, Word32)
--          collatzMax previous candidate =
--                max previous (collatzLength candidate, candidate)

--          -- The accumulator is of the form '(length, start_value)' of the
--          -- maximum entry so far.
--          maxTuple :: [Word32] -> (Word16, Word32)
--          maxTuple = foldl collatzMax (1, 1)



{-
      Problem 15
            How many routes are there through a 20*20 grid?

      Result
            137846528820
            0.00 s

      Comment
            To traverse the 20*20 grid, one has to take 20 steps right and
            another 20 down. The problem thus reduces in "how many different
            ways are there to arrange right and down", which is simply the
            binomial coefficient.
-}
euler 15 = Just $ (20 + 20) `choose` 20


{-
      Problem 16
            Digit sum of 2^1000?

      Result
            1366
            0.00s
-}
euler 16 = Just . digitSum 10 $ 2^1000



{-
      Problem 17
            How many letters are used when counting from 1 to 1000 in (British)
            english?

      Result
            21224
            .006 s
-}
euler 17 = Just . sum' $ map numberLength [1..1000]
      where numberLength = genericLength . filter isLetter . spellNumber
            spellNumber n
                  | n ==   0  = ""
                  | n ==   1  = "one"
                  | n ==   2  = "two"
                  | n ==   3  = "three"
                  | n ==   4  = "four"
                  | n ==   5  = "five"
                  | n ==   6  = "six"
                  | n ==   7  = "seven"
                  | n ==   8  = "eight"
                  | n ==   9  = "nine"
                  | n ==  10  = "ten"
                  | n ==  11  = "eleven"
                  | n ==  12  = "twelve"
                  | n ==  13  = "thirteen"
                  | n ==  14  = "fourteen"
                  | n ==  15  = "fifteen"
                  | n ==  16  = "sixteen"
                  | n ==  17  = "seventeen"
                  | n ==  18  = "eighteen"
                  | n ==  19  = "nineteen"
                  | n <=  29  = "twenty"  ++ spellNumber (n `rem` 10) -- There's a space
                  | n <=  39  = "thirty"  ++ spellNumber (n `rem` 10) --  missing, e.g.
                  | n <=  59  = "fifty"   ++ spellNumber (n `rem` 10) --  "twentyone", but
                  | n <=  69  = "sixty"   ++ spellNumber (n `rem` 10) --  since the result
                  | n <=  79  = "seventy" ++ spellNumber (n `rem` 10) --  is independent of
                  | n <=  89  = "eighty"  ++ spellNumber (n `rem` 10) --  this, I haven't
                  | n <=  99  = "ninety"  ++ spellNumber (n `rem` 10) --  adressed the issue
                  | n <= 999  = let (q,r) = n `quotRem` 100
                                    andWord = if r /= 0 then " and " else ""
                                in  spellNumber q ++ " hundred" ++ andWord ++ spellNumber r
                  | otherwise = "one thousand"



{-
      Problem 18
            Maximum sum in a triangle of numbers

      Solution idea
            Reduce the triangle row by row, starting from the bottom. For
            example, the bottom right corner is the triangle [[31],[04,23]]. If
            one reaches the 31, the maximizing step is taking 23, 04 is not an
            option. Therefore, the cell with 31 can effectively be considered a
            cell on the bottom of the triangle with value 31+23=54. Doing this
            for all 3-digit triangles in the bottom row reduces the triangle by
            one row; iterating the process until the tip of the pyramid has been
            reached yields the desired result.

      Result
            1074
            .01 s
-}
euler 18 = listToMaybe $ foldr1 rowReduce P18.triangle
      where -- rowReduce compares all the small triangles in the bottom rows,
            -- and reduces the two lines to one effective line. Folding this
            -- over the pyramid yields a singleton list containing the result.
            -- Example: rowReduce [1,2] [7,1,4] -> [8,6]
            rowReduce upper lower = zipWith max lShifted rShifted
                  where lShifted = zipWith (+) upper (init lower) -- init unnecessary, but makes the code nicely symmetric :-)
                        rShifted = zipWith (+) upper (tail lower)
            -- -- Golfed version:
            -- -- (Maybe some more spaces can be omitted)
            -- rowReduce u l=z max (p id)$p tail
            --    where p f=z (+) u$f l
            --          z=zipWith



{-
      Problem 20
            Digit sum of 100!?

      Result
            648
            0.00 s
-}
euler 20 = Just . digitSum 10 . faculty $ 100


{-
      Problem 21
            Find the sum of all the amicable numbers under 10000.
            (Amicable == d^2 = id; d = sum of proper divisors

      Solution
            31626

      Result
            1.84 s
-}
euler 21 = Just . sum' . filter isAmicable $ [1..maxN]
      where maxN = 10^4
            d = sum' . properDivisors
            isAmicable n =
                  let dn = d n
                  in  dn <= maxN && dn /= n && d dn == n



{-
      Problem 22
            Process a file with a lot of names

      Result
            .09 s
-}
euler 22 = Just $ nameListScore P22.names
      where
            -- Calculates the total score of a list of names.
            -- The score consists of the position of the name in the sorted list
            -- (starting with 1) multiplied by the name score.
            -- Example: "ABD" at position 8 yields (1+2+4)*8 = 56.
            nameListScore :: (Enum a, Num a) => [String] -> a
            nameListScore names = sum' . zipWith (*) [1..] . map nameScore . sort $ names

            -- Calculates the name score of a single name.
            -- Example: "ABD" = 1+2+4 = 7
            nameScore :: (Num a) => String -> a
            nameScore name = fromIntegral . sum' $ map (\x -> ord x - ord 'A' + 1) name



{-
      Problem 25
            Index of the first Fibonacci number with 1000 digits in the
            Fibonacci sequence?

      Result
            4782
            0.11 s
-}
-- euler 25 = Just . fst . fromJust $ find (has1000 . snd) fiboPairs
euler 25 = fst <$> find (has1000 . snd) fiboPairs
      where fiboPairs = zip [1..] fibo -- Pairs of (n, F_n)
            has1000 n = n >= 10^999    -- Does n have 1000 digits?



{-
      Problem 28
            Find the sum of corners in a spiral list

      Result
            669171001
            0.00 s
-}
euler 28 = Just $ sum' spiralList
      where
            -- List of offsets to the next number.
            -- = [1, 2,2,2,2, 4,4,4,4, 6,6,6,6, ...]
            spiralOffsets = 1 : ([2,4..1000] >>= replicate 4)

            -- Accumulate offsets to generate the actual list of spiral elements
            spiralList = scanl1 (+) spiralOffsets



{-
      Problem 29
            How many distinct values of a^b for 2 <= a,b <= 100 are there?

      Result
            9183
            .94 s
-}
euler 29 = Just $ numDistinct powers
      where numDistinct = genericLength . group . sort
            powers = liftM2 (^) [2..100] [2..100]



{-
      Problem 30
            Numbers that can be written as powers of their digits

      Result
            443839
            6.28 s

      Comment
            The upper boundary can be estimated since 999... = 10^k - 1 has to
            be equal to 9^5 + 9^5 + ... = k 9^5, which yields the maximum
            condition k 9^5 = 10^k - 1. A numeric solution for this is 5.51257,
            which yields a maximum of 10^5.51257 = 325514.24.
-}
euler 30 = Just . fromIntegral . sum' $ filter is5thPowerSum [2..325515]
      where
            -- Can x be written as the sum of fifth power of its digits?
            is5thPowerSum x = x == (sum' . map toTheFifth $ show x)

            -- Memoize powers
            toTheFifth '0' = 0^5
            toTheFifth '1' = 1^5
            toTheFifth '2' = 2^5
            toTheFifth '3' = 3^5
            toTheFifth '4' = 4^5
            toTheFifth '5' = 5^5
            toTheFifth '6' = 6^5
            toTheFifth '7' = 7^5
            toTheFifth '8' = 8^5
            toTheFifth '9' = 9^5



{-
      Problem 33
            Discover all the fractions with an unorthodox cancelling method, and
            find the denominator of their product.

      Result
            100
            0.03 s
-}
euler 33 = Just . denominator . product' $ do
      b <- [1..9] -- 3 instances of this isn't enough to justify using sequence
      c <- [1..9]
      d <- [1..9]
      a <- [1..c-1] -- ensures numerator < denominator
      let frac  = (10 * a + b) % (10 * c + d)
      guard $ b == c && a % d == frac
              ||
              a == d && b % c == frac
      return frac



{-
      Problem 34
            Numbers that can be written as factorials of their digits

      Result
            54 s

      Comment
            Similar estimate as in problem 30 results in a numerical upper
            boundary estimate of 2309192.
-}
euler 34 = Just . fromIntegral . sum' $ filter isSumOfFacDigits [3..2309192]
      where
            -- f x = can x be written as the sum of faculties of its digits?
            isSumOfFacDigits x = x == (sum' . map faculty' $ show x)

            -- This is even (much) faster than using a vector lookup table :-)
            faculty' '0' = 1
            faculty' '1' = 1
            faculty' '2' = 2
            faculty' '3' = 6
            faculty' '4' = 24
            faculty' '5' = 120
            faculty' '6' = 720
            faculty' '7' = 5040
            faculty' '8' = 40320
            faculty' '9' = 362880



{-
      Problem 35
            How many circular primes are there below a million?

      Result
            55
            87 s
-}
euler 35 = Just . genericLength $ filter isCircularPrime candidates
      where candidates = takeWhile (< 10^6) Primes.primes
            isCircularPrime p = all Primes.isPrime rotations
                  where digits = explodeInt10 p
                        rotateList [] = []
                        rotateList (x:xs) = xs ++ [x]
                        rotations = map (implodeInt 10) .
                                    take (length digits) $
                                    iterate rotateList digits



{-
      Problem 36
            Find the sum of all numbers below 10^6 that are palindromic in
            base 2 and 10.

      Result
            18 s
-}
euler 36 = Just . sum' $ filter isDoublePali [1..10^6-1]
      where
            isDoublePali n = isBasePali 10 n && isBasePali 2 n
            isBasePali 10 n = let exploded = show n
                              in  exploded == reverse exploded
            isBasePali  b n = let exploded = reverseExplodeInt b n
                              in  exploded == reverse exploded



{-
      Problem 37
            Find all 11 primes that are left- and right-truncatable

      Result
            748317
            2.7 s
-}
euler 37 = Just . sum' . take 11 . filter isTruncatable $ candidates
      where
            candidates = dropWhile (<= 7) Primes.primes -- exclude single digit primes
            isTruncatable n = allPrimes truncates
                  where
                        n' = explodeInt10 n
                        allPrimes = all $ Primes.isPrime . implodeInt 10
                        truncates = (middle $ inits n') ++ (middle $ tails n')
                        middle = init . tail -- Everything but head and last
                                             -- (since inits xs = [xs ... []])



{-
      Problem 40
            Product of decimals in a long number

      Result
            210
            2.2 s
-}
euler 40 = Just . product' $ map (concatInts !!) d_n
      where
            -- Concatenated decimals
            -- = [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1,5,...]
            concatInts = [1..] >>= explodeInt10

            -- Indices of d to be considered
            -- = [1, 10, 100, 1000, 10000, 100000, 1000000]
            d_n = [10^n - 1 | n <- [0..6]] -- -1 because lists are 0-indexed



{-
      Problem 41
            Find the largest pandigital prime

      Result
            0.01 s

      Comment
            "Why does the search start with a 7-pandigital and not one of rank 9?
            All n-pandigitals of a certain order have the same digit sum:
                  n = 9 => digit sum 45
                  n = 8 => digit sum 36
                  n = 7 => digit sum 28
                  ...
            This shows that n = 8,9 cannot be pandigital primes, as their digit
            sum is divisible by 3. It is also known that there is a pandigital
            prime because the problem gives 2143 as an example, which is a
            4-pandigital. The rest of the algorithm is brute force.
-}
euler 41 = Just . fromIntegral . prevPanPrime $ Permute.listPermute 7 [6,5..0]
      where prevPanPrime p
                  | Primes.isPrime n = n
                  | otherwise = prevPanPrime . fromMaybe prunedP $ Permute.prev p
                  where
                        n = getNumber p
                        -- When the algorithm reaches the smallest n-pandigital,
                        -- this generates the permutation for the largest
                        -- (n-1) pandigital. Example: p = 123 => prunedP = 21
                        -- Note that this is not actually needed here, since
                        -- there is a 7-digit pandigital, but you can't know
                        -- that in advance :-)
                        prunedP = Permute.listPermute (length ipe) $ reverse ipe
                        ipe = init . Permute.elems $ p

                        -- Extract current number out of a permutation
                        getNumber = implodeInt 10 . map (+1) . Permute.elems



{-
      Problem 42
            Counting triangle words

      Result
            162
            0.04 s
-}
euler 42 = Just . genericLength $ triangleWords
      where triangleNumbers = [n * (n+1) `quot` 2 | n <- [1..]]
            isTriangle n = n == (fromJust . find (>= n) $ triangleNumbers)
            charValue c = ord c - ord 'A' + 1 -- A = 1, B = 2, ...
            wordValue = sum' . map charValue
            triangleWords = filter (isTriangle . wordValue) $ P42.candidates



{-
      Problem 48
            Find the last ten digits of 'sum n^n'.

      Result
            9110846700
            .03 s

      Comment
            The heart of this is using an efficient power implementation. The
            one provided by the (self-written) Power library computes it in
            O(log(n)), but the naive usage still produces very large inter-
            mediate numbers. However, by truncating every intermediate result
            of the multiplications required to calculate the powers gets rid
            of this issue, and the algorithm scales well.
            (Profiling shows that the naive implementation, shown below the
            following code, outperforms this code by a small margin. However,
            if the maximum n is increased to 10^4, it already loses by an order
            of magnitude, and 10^5 took so long I aborted it. The truncation-
            based version still finished quickly.)
-}
euler 48 = Just . last10 $ sum' [n ^. n | n <- [1..1000]]
      where last10 x = x `rem` 10^10
            a ^. b = Power.power (\x y -> last10 $! x * y) b a



{-
      Problem 50
            Which prime under a million can be written as the sum of the most
            consecutive primes?

      Result
            997651
            24.21 s
-}
euler 50 = Just . snd . maximumBy (comparing fst) $ primeSums
      where
            pMax = 10^6
            primes' = tails $ takeWhile (< pMax) Primes.primes
            primeSums = concatMap (filterCrap . scanl ls (0,0)) primes'
                  where ls (!l, !s) x = (l + 1, s + x) -- ls = length/sum
            -- Stop summing if the sum becomes too large
            filterCrap = filter (Primes.isPrime . snd) . takeWhile (\x -> snd x < pMax)



{-
      Problem 53
            How many binomial coefficients with 1 <= n <= 100 exceed 10^6?

      Result
            4075
            0.25 s
-}
euler 53 = Just $ sum' $ do
      n <- [23..100] -- By mathematical analysis, n = 23 is the first value for
                     -- which the binomial coefficient can exceed 10^6.
      let nHalf = n `quot` 2 -- Ceiled half of n
      k <- [2..nHalf]
      -- Use 'choose n k == choose n (n-k)' to count certain values twice
      -- instead of recomputing both instances separately:
      let weight | k == nHalf && odd n = 1 -- Only count the middle point once
                 | otherwise  = 2 -- In other cases, there is no single middle
                                  -- point, hence the "counting a single value
                                  -- double" problem does not occur
      guard $ n `choose` k > 10^6
      return weight



{-
      Problem 56
            Considering natural numbers of the form a^b, where a, b < 100, what
            is the maximum digital sum?

      Result
            1.8 s
-}
euler 56 = Just . maximum' . map (digitSum 10) $ [a^b | a <- [1..100], b <- [1..100]]



{-
      Problem 59
            Decrypt an English text ciphered using a XOR'd key
to put one's foot in one's mouth
      Result
            107359
            0.05 s s

      Comment:
            Initialy, I solved this by checking how many times the most common
            ~20 words appear in the decyphered text. However, it turns out that
            it is sufficient to count the spaces in the text for the simple
            reason that spaces occur much more frequently in written than in
            random text (every ~10 characters vs. every 128 ASCII chars).
            The problem now is that this still leaves 26^3 different possible
            keys (runtime in GHCi: around 18 s).
            Next, note that the problem is actually decyphering three
            independent messages: every (3n) char, (3n+1), (3n+2), each with a
            one-char key. Decyphering each of them individually, again by
            heuristically counting spaces, reveals the message not in 26^3 but
            in 26*3 attempts.
-}
euler 59 = Just . fromIntegral . sum' . decrypt $ message
      where
            message = P59.message

            keyCandidates = map ord ['a'..'z']

            messageSplit = msgSplit message 0 [[],[],[]]
                  where
                        msgSplit [] _ result = result
                        msgSplit (x:xs) n [a,b,c]
                              | n == 0 = msgSplit xs 1 [x:a,  b,  c]
                              | n == 1 = msgSplit xs 2 [  a,x:b,  c]
                              | n == 2 = msgSplit xs 0 [  a,  b,x:c]

            decryptOne msg keyChar = map (`xor` keyChar) msg

            spaceCount = foldl go 0
                  where go :: (Integral i) => i -> Int -> i
                        go !count char
                              | char == sp = count + 1
                              | otherwise  = count
            sp = ord ' '

            bestKeyChar msg = fst . maximumBy (comparing snd) . map go $ keyCandidates
                  where go k = (k, spaceCount $ decryptOne msg k)

            bestKey = map bestKeyChar messageSplit

            decrypt msg = zipWith xor msg (cycle bestKey)



-- Code copied from problem 18. Do not modify the code here. Performance: .05 s
euler 67 = listToMaybe $ foldr1 rowReduce P67.triangle
      where rowReduce upper lower = zipWith max lShifted rShifted
                  where lShifted = zipWith (+) upper (init lower)
                        rShifted = zipWith (+) upper (tail lower)



{-
      Problem 71
            Find the numerator of the fraction before 3/7 in the Farey sequence
            for d <= 10^6.

      Result
            428570
            1.05 s

      Comment
            This algorithm was constructed using the small note from [1], which
            reads "For a method of computing a successive sequence from an
            existing one of n terms, insert the mediant fraction (a+b)/(c+d)
            between terms a/c and b/d when c+d<=n".

            This is *very* brief, so here's an elaboration on that:

            The mediant of two fractions a/b and c/d is (a+b)/(c+d). This
            fraction has two properties that are important here:
                  1. a/b < mediant < c/d
                  2. The mediant's denominator is the smallest denominator of
                     all the (reduced) fractions that lie between a/b and c/d.
            We're now looking for the fraction before 3/7 in the Farey Sequence.
            Calculating the mediant of these two has two possible outcomes:
                  x) The mediant has a denominator greater than n, the index
                     of the Farey sequence. Since we know from 1. that it's
                     between the two fractions we put in and from 2. that it's
                     the one with the smallest possible denominator, we can
                     conclude that this mediant is not part of the Farey
                     Sequence. Therefore, the two input fractions are adjacent.
                  y) The mediant is in the Farey Sequence and the denominator
                     is not too large. From 1. we can be sure that it's
                     strictly between the two input fractions. If we now change
                     the first input fraction to the generated mediant and
                     recurse, the algorithm is called again with a narrower
                     focus. Repeating this will eventually yield case x), and
                     the algorithm terminates.
            So to calculate the fraction before 3/7, the algorithm moves
            the left boundary towards 3/7, until it detects that there's no
            fraction between the boundaries, in which case two neighbours have
            been found.

            A final note: using self-made strict tuples makes this a little bit
            faster than using Data.Ratio for the fractions.
-}
euler 71 = Just . fst $ fareyBefore (10^6) (1,7) (3,7) -- TODO: should 0 be (0,1) instead? 0::Ratio Int == 0%1
      where fareyBefore n ac@(!a,!c) bd@(!b,!d)
                  | c + d > n = ac
                  | otherwise = fareyBefore n abcd bd
                  where
                        ab = a + b
                        cd = c + d
                        gcd' = gcd ab cd
                        abcd = (ab `quot` gcd', cd `quot` gcd')

{-
Here's an algorithm to calculate the full Farey sequence F_n, deduced from
http://en.wikipedia.org/wiki/Farey_sequence#Next_term

fareySequence :: (Integral a) => a -> [Ratio a]
fareySequence n = 0 : unfoldr nextFarey (0 % 1, 1 % n)
      where
            -- Computes the next fraction out of the predecessors
            -- a%b and c%d.
            -- Reference: http://en.wikipedia.org/wiki/Farey_sequence#Next_term
            nextFarey (ab, cd)
                  | ab == 1 = Nothing
                  | otherwise = Just (cd, (cd, pq))
                  where
                        (a,b) = (numerator ab, denominator ab)
                        (c,d) = (numerator cd, denominator cd)
                        pq = p % q
                        k = (n + b) `div` d
                        p = k * c - a
                        q = k * d - b
-}




{-
      Problem 73
            How many reduced fractions are between 1/3 and 1/2 with maximum
            denominator 12000?

      Result
            7295372
            108 s

      Comment
            1. See problem 71 for more details on this, namely Farey Sequences.
            2. Using tuples (numerator,denominator) makes this 3 times faster
               compared to using Data.Ratio. (Maybe because my mediant is
               strict?)
-}
euler 73 = Just . fromIntegral $ fareyCount 12000 (1,3) (1,2)
      where mediant (!a,!b) (!c,!d) = (ac', bd')
                  where ac   = a + c
                        bd   = b + d
                        gcd' = gcd ac bd
                        ac'  = ac `quot` gcd'
                        bd'  = bd `quot` gcd'

            -- n: max denominator, ab/cd: fractions to search between
            fareyCount :: Int       -- ^ Max denominator
                       -> (Int, Int) -- ^ Lower boundary
                       -> (Int, Int) -- ^ Upper boundary
                       -> Int
            fareyCount n ab cd
                  | d > n = 0
                  | otherwise = 1 + fareyCount n ab m + fareyCount n m cd
                  where m@(_,d) = mediant ab cd -- Mediant/denominator



{-
      Problem 97
            Find the last 10 digits of the prime 28433 * 2^7830457 + 1.

      Result
            8739992577
            0.01 s

      Comment
            The key is efficient calculation of the power. Using the (self-
            written) Power package, the number of multiplications scales like
            log(exponent). The multiplications at each step have to be carried
            out for the last 10 digits, as the other digits don't influence the
            last digits in subsequent calculations.
-}
euler 97 = Just . last10 $ 28433 * 2^.7830457 + 1
      where last10 x = x `rem` 10^10
            infixl 8 ^.
            a ^. b = Power.power (\x y -> last10 $! x * y) b a



{-
      Problem 99
            Comparing large numbers of the form base^exponent

      Result
            709
            0.04 s

      Comment
            It is sufficient to compare the logs of the values, as log preserves
            order.
-}
euler 99 = Just . fst . maxSnd . zip [1..] $ map log' P99.baseExpTuples
      where maxSnd = maximumBy . comparing $ snd
            log' ~(b, e) = fromIntegral e * log (fromIntegral b)



-- Problem hasn't been solved yet
euler _ = Nothing



-- == PLAYGROUND ==
