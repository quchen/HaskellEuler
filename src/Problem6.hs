{-
      Problem 6
            Find the difference between the sum of the squares of the first one
            hundred natural numbers and the square of the sum.

      Result
            25164150
            .00 s
-}
module Problem6 (solution) where

solution = (n-1) * n * (1+n) * (2+3*n) `div` 12
      where n = 100