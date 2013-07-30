{-
      Problem 12
            What is the value of the first triangle number to have over five
            hundred divisors?

      Result
            76576500
            0.74 s
-}
module Problem12 (solution) where

import Data.List
import Data.Numbers.Primes

solution = head [triangle n | n <- [1..], triangleD n > 500]
      where -- Calculates the n-th triangle number
            triangle n = n * (n+1) `quot` 2

            -- Number of divisors of the triangle number n*(n+1)/2. Uses the
            -- fact that n and (n+1) are coprime, so the divisor count is a
            -- homomorphism on them.
            triangleD n
                  | even n    = numDivisors (n `quot` 2) * numDivisors (n + 1)
                  | otherwise = numDivisors n * numDivisors ((n + 1) `quot` 2)

            -- Number of divisors.
            -- Based on the fact that a number with prime factors of multi-
            -- plicities a1, a2, ... has (a1+1)(a2+1)... divisors.
            -- Equivalent to length.divisors, but performs way better.
            numDivisors = product . map (+1) . pfm
            -- pfm = prime factor multiplicities
            pfm = map length . group . primeFactors