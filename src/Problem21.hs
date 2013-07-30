{-
      Problem 21
            Find the sum of all the amicable numbers under 10000.
            (Amicable == d^2 = id; d = sum of proper divisors

      Solution
            31626

      Result
            1.84 s
-}
module Problem21 (solution) where

import CommonFunctions

solution = sum' . filter isAmicable $ [1..maxN]
      where maxN = 10^4
            d = sum' . properDivisors
            isAmicable n = let dn = d n
                           in  dn <= maxN && dn /= n && d dn == n