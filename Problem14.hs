{-# LANGUAGE BangPatterns #-}

{-
      Problem 14
            Which starting number, under one million, produces the longest
            Collatz chain?

      Result
            837799
            10 s
-}
module Problem14 (solution) where

import Data.Array

solution = fst maxTuple
      where maxN :: Integer
            maxN = 10^6-1

            -- How many numbers to memoize.
            -- Too large consumes too much space, too little and memoization
            -- is useless.
            memoN :: Integer
            memoN = maxN

            -- Vector of the type [(n, collatz steps n has to take to reach loop)],
            -- intended to memoize already calculated lengths below a million.
            -- The first element is (1,1), since 1 has to take 1 step to loop.
            collatzLengthList :: Array Integer Integer
            collatzLengthList = array (1, memoN) [(n, collatzLength collatzLengthList n 1) | n <- [1..memoN]]
            -- collatzLengthList = V.generate (maxN+1) (\n -> if n == 0 then (0,-1) else (n, collatzLength collatzLengthList n 1))

            collatzLength :: Array Integer Integer -> Integer -> Integer -> Integer
            collatzLength memo n c
                  | n == 1                                           = 1
                  | even n && bounds memo `inRange` (n `quot` 2) = c + (memo ! (n `quot` 2))
                  | odd  n && bounds memo `inRange`  (3 * n + 1) = c + (memo ! (3 * n +  1))
                  | even n                                           = collatzLength memo (n `quot` 2) (c+1)
                  | otherwise                                        = collatzLength memo (3*n + 1) (c+1)

            maxTuple = foldl1 maxC . take (fromIntegral maxN) $ assocs collatzLengthList
                  where maxC a@(_,!a2) x@(_,x2)
                              | a2 >= x2  = a
                              | otherwise = x