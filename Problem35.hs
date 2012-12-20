{-
      Problem 35
            How many circular primes are there below a million?

      Result
            55
            87 s
-}
module Problem35 where

import Data.List
import Data.Numbers.Primes
import CommonFunctions

solution = genericLength $ filter isCircularPrime candidates
      where candidates = takeWhile (< 10^6) primes

isCircularPrime p = all isPrime rotations
      where digits = explodeInt10 p
            rotations = map (implodeInt 10) .
                        take (length digits) $
                        iterate rotateList digits

rotateList [] = []
rotateList (x:xs) = xs ++ [x]