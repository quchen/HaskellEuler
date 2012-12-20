{-
      Problem 1
            Find the sum of all the multiples of 3 or 5 below 1000.

      Result
            233168
            .01 s

      Comment
            The formula below is based on sum [1..n] == n(n+1)/2.
-}
module Problem1 where

solution = (sumFor 3 + sumFor 5 - sumFor 15) `quot` 2
      where nMax = 1000 - 1
            sumFor n = let nX = nMax `quot` n
                       in  n * nX * (nX + 1)