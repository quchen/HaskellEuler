{-
      Problem 31
            How many ways are there to make change for £2?

      Result
            73682
            7.45 s
-}

module Problem31 (solution) where

import Data.List (genericLength)

change amount = go amount numCoins
      where
            go 0 _         = 1
            go _ 0         = 0
            go n _ | n < 0 = 0
            go n c         = go n (c-1) + go (n - coin c) c

coins = [1, 2, 5, 10, 20, 50, 100, 200]
coin n = coins !! (n - 1)
numCoins = genericLength coins

solution = Just $ change 200