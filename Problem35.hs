{-
      Problem 35
            How many circular primes are there below a million?

      Result
            55
            18 s
-}
module Problem35 (solution) where

import Data.List
import Data.Numbers.Primes
import CommonFunctions

solution = length' $ filter isCircularPrime candidates
      where candidates = takeWhile (< 10^6) primes

-- isCircularPrime p = all isPrime rotations
--       where digits = explodeInt10 p
--             rotations = map (implodeInt 10) .
--                         take (length digits) $
--                         iterate rotateList digits

-- rotateList [] = []
-- rotateList (x:xs) = xs ++ [x]


isCircularPrime p = all isPrime rotations
      where digits = explodeInt10 p
            lengthDigits = length digits
            rotations = map (implodeInt 10) .
                        map (take lengthDigits) .
                        take lengthDigits .
                        tails $
                        cycle digits