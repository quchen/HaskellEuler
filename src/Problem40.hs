{-
      Problem 40
            Product of decimals in a long number

      Result
            210
            2.2 s
-}
module Problem40 (solution) where

import CommonFunctions
import Control.Monad
import Control.Monad.RWS

solution = product' $ map (concatInts !!) d_n
      where
            -- Concatenated decimals
            -- = [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1,5,...]
            concatInts = [1..] >>= explodeInt10

            -- Indices of d to be considered
            -- = [1, 10, 100, 1000, 10000, 100000, 1000000]
            d_n = [10^n - 1 | n <- [0..6]] -- -1 because lists are 0-indexed