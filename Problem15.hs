{-
      Problem 15
            How many routes are there through a 20*20 grid?

      Result
            137846528820
            0.00 s

      Comment
            To traverse the 20*20 grid, one has to take 20 steps right and
            another 20 down. The problem thus reduces in "how many different
            ways are there to arrange right and down", which is simply the
            binomial coefficient.
-}
module Problem15 (solution) where

import CommonFunctions

solution = (20 + 20) `choose` 20