{-
      Problem 36
            Find the sum of all numbers below 10^6 that are palindromic in
            base 2 and 10.

      Result
            18 s
-}
module Problem36 (solution) where

import CommonFunctions

solution = sum' $ filter isDoublePali [1..10^6-1]

isDoublePali n = isBasePali 10 n && isBasePali 2 n
isBasePali 10 n = let exploded = show n
                  in  exploded == reverse exploded
isBasePali  b n = let exploded = reverseExplodeInt b n
                  in  exploded == reverse exploded