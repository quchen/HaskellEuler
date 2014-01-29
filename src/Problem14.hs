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

import qualified Data.Vector as V
import Data.Ix

solution = fromIntegral (V.maxIndex collatzLengthTable + 1)
      where maxN :: Int
            maxN = 10^6

            -- Vector of the type [(n-1, collatz steps n has to take to reach loop)]
            -- to memoized previously obtained lengths.
            collatzLengthTable :: V.Vector Int
            collatzLengthTable = V.generate maxN gen
                  where gen n = collatzLength collatzLengthTable (n+1) 0

            -- Check whether the Collatz length of number n is memoized
            isMemoized n = n <= V.length collatzLengthTable

            collatzLength :: V.Vector Int -- ^ Memo table
                          -> Int -- ^ Current number
                          -> Int -- ^ Number of steps to count to here
                          -> Int -- ^ Length of the chain to get to 1
            collatzLength memo n !count
                  | n == 1 = count
                  | even n && isMemoized nHalf   = count + memo !^ nHalf
                  | odd  n && isMemoized n3plus1 = count + memo !^ n3plus1
                  | even n    = collatzLength memo nHalf   (count+1)
                  | otherwise = collatzLength memo n3plus1 (count+1)
                  where nHalf   = n `quot` 2
                        n3plus1 = 3*n + 1

            -- Ix-based vector indexing
            vec !^ i = vec V.! index (1, maxN) i