{-
      Problem 25
            Index of the first Fibonacci number with 1000 digits in the
            Fibonacci sequence?

      Result
            4782
            0.11 s
-}
module Problem25 (solution) where

import Data.List
import CommonFunctions
import Data.Maybe

solution = fst . fromJust $ find (has1000 . snd) fiboPairs
      where fiboPairs = zip [1..] fibo -- Pairs of (n, F_n)
            has1000 n = n >= 10^999    -- Does n have 1000 digits?