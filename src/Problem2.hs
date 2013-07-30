{-
      Problem 2
            Find the sum of the even-valued Fibonacci numbers <= 4e6.

      Result
            .00 s
-}
module Problem2 (solution) where

import CommonFunctions

solution = sum' . filter even $ takeWhile (<= 4 * 10^6) fibo