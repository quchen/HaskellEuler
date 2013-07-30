{-
      Problem 37
            Find all 11 primes that are left- and right-truncatable

      Result
            748317
            2.7 s
-}
module Problem37 (solution) where

import Data.Numbers.Primes
import Data.List
import CommonFunctions

solution = sum' . take 11 . filter isTruncatable $ candidates
      where candidates = dropWhile (<= 7) primes -- exclude single digit primes


isTruncatable n = allPrimes truncates
      where n' = explodeInt10 n
            allPrimes = all $ isPrime . implodeInt 10
            truncates = middle (inits n') ++ middle (tails n')
            middle = init . tail -- Everything but head and last
                                 -- (since inits xs = [xs ... []])