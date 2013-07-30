{-
      Problem 43
            Numbers with unusual sub-number divisibility property

      Result
            16695334890
            61 s
-}

module Problem43 (solution) where

import Data.Permute
import Data.List
import CommonFunctions

-- prop 1 2 [1,2,3,4] == 2 divides 234?
prop n d number = (implodeInt 10 . take 3 . drop n $ number) `rem` d == 0

specialNumber n = and [ prop 1  2 n
                      , prop 2  3 n
                      , prop 3  5 n
                      , prop 4  7 n
                      , prop 5 11 n
                      , prop 6 13 n
                      , prop 7 17 n
                      ]

pandigitals = map permDigits $ idPerm : unfoldr nextPerm idPerm
      where idPerm = listPermute 10 [0..9]
            nextPerm p = maybe Nothing (\next -> Just (next,next)) $ next p

            permDigits :: Permute -> [Integer]
            permDigits = map fromIntegral . elems

solution = sum' . map (implodeInt 10) . filter specialNumber $ pandigitals
