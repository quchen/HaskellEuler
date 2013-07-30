{-
      Problem 49
            Primes whose digits are permutations of each other

      Solution
            UNSOLVED
-}

import Data.Numbers.Primes
import Data.List
import Data.Ord
import Data.Function
import CommonFunctions

fourDigitPrimes = takeWhile (< 10^4) . dropWhile (< 10^3) $ primes

sort4DigitPrimes = sortBy (comparing sortPrime) fourDigitPrimes

group4DigitPrimes = groupBy ((==) `on` sortPrime) sort4DigitPrimes

sortPrime = sort . explodeInt 10

triples = filter ((==) 3 . length) group4DigitPrimes

solution = filter (\ ~[a,b,c] -> c-b == b-a) triples