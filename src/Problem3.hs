{-
      Problem 3
            The prime factors of 13195 are 5, 7, 13 and 29.
            What is the largest prime factor of the number 600851475143?

      Result
            6857
            .01 s
-}
module Problem3 (solution) where

import Data.Numbers.Primes

solution = last . primeFactors $ 600851475143