{-
      Problem 31
            How many ways are there to make change for £2?

      Result
            73682
            0.02 s
-}

module Problem31 (solution) where

import CommonFunctions (length')
import Data.Array

-- Solution inspired by "counting change" from Structure and Interpretation of
-- Computer Programs by Abelson, Sussman, Sussman
change amount = go amount numCoins
      where
            go 0 _         = 1
            go _ 0         = 0
            go n _ | n < 0 = 0
            go n c         = goMemo n (c-1) + goMemo (n - coin c) c

            goMemo n c | inRange boundsMemo (n,c) = memo ! (n,c)
                       | otherwise = go n c
            boundsMemo = ((1,1), (amount, numCoins))
            memo = array boundsMemo
                         [((n,c), go n c) | n <- [1..amount],
                                            c <- [1..numCoins]]

coins = [1, 2, 5, 10, 20, 50, 100, 200]
numCoins = length' coins
coinsArray = listArray (1, numCoins) coins
coin n = coinsArray ! n

solution = change 200