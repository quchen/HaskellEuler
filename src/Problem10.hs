{-
      Problem 10
            Find the sum of all primes <= 2 * 10^6

      Result
            142913828922
            0.55 s
-}
module Problem10 (solution) where

import CommonFunctions
import Data.Numbers.Primes

solution = sum' . takeWhile (<= 2 * 10^6) $ primes