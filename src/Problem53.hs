{-
      Problem 53
            How many binomial coefficients with 1 <= n <= 100 exceed 10^6?

      Result
            4075
            0.25 s
-}

module Problem53 (solution) where

import Control.Monad
import CommonFunctions

solution = sum' $ do
      n <- [23..100] -- By mathematical analysis, n = 23 is the first value for
                     -- which the binomial coefficient can exceed 10^6.
      let nHalf = n `quot` 2 -- Floored half of n
      k <- [2..nHalf]
      -- Use 'choose n k == choose n (n-k)' to count certain values twice
      -- instead of recomputing both instances separately:
      let weight | k == nHalf && odd n = 1 -- Only count the middle point once
                 | otherwise  = 2 -- In other cases, there is no single middle
                                  -- point, hence the "counting a single value
                                  -- double" problem does not occur
      guard $ n `choose` k > 10^6
      return weight