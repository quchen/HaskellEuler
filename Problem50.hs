{-# LANGUAGE BangPatterns #-}

{-
      Problem 50
            Which prime under a million can be written as the sum of the most
            consecutive primes?

      Result
            997651
            24.21 s
-}

module Problem50 (solution) where

import Data.Numbers.Primes
import Data.List
import CommonFunctions
import Data.Ord



solution = snd . maximumBy (comparing fst) $ primeSums

pMax = 10^6

-- Tails of list of primes below pMax
primes' = tails $ takeWhile (< pMax) primes

primeSums = concatMap (filterCrap . scanl ls (0,0)) primes'
      where ls (!l, !s) x = (l + 1, s + x) -- ls = length/sum

-- TakeWhile to stop summing if the sum becomes too large
filterCrap = filter (isPrime . snd) . takeWhile ((< pMax) . snd)