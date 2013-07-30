{-
      Problem 7
            What is the 10001st prime number?

      Performance:
            104743
            .04 s
-}
module Problem7 (solution) where

import Data.Numbers.Primes

solution = primes !! (10001-1) -- Lists start counting at 0