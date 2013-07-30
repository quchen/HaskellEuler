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
module Problem5 (solution) where

import Data.List

solution = foldl1' lcm' [1..20]
      where lcm' a b = a `quot` gcd' a b * b
            gcd' a 0 = a
            gcd' a b = gcd' b (a `rem` b)