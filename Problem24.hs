{-
      Problem 24
            What is the 10^6th permutation of 0123456789?

      Result
            2783915460
            4.42 s
-}

module Problem24 (solution) where

import Data.Permute
import CommonFunctions
import Data.Maybe

start = listPermute 10 [0..9]

nextN :: (Integral i) => i -> Permute -> Maybe Permute
nextN n p | n == 0    = Just p
          | otherwise = next p >>= nextN (n-1)

millionthPermutation = fromJust . nextN (10^6-1) $ start

solution :: Integer
solution = implodeInt 10 . map fromIntegral . elems $ millionthPermutation