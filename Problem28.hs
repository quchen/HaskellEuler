{-
      Problem 28
            Find the sum of corners in a spiral list

      Result
            669171001
            0.00 s
-}
module Problem28 (solution) where

import Data.List
import CommonFunctions

solution = sum' spiralList

-- List of offsets to the next number.
-- = [1, 2,2,2,2, 4,4,4,4, 6,6,6,6, ...]
spiralOffsets = 1 : ([2,4..1000] >>= replicate 4)

-- Accumulate offsets to generate the actual list of spiral elements
spiralList = scanl1 (+) spiralOffsets