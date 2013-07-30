{-
      Problem 20
            Digit sum of 100!?

      Result
            648
            0.00 s
-}
module Problem20 (solution) where

import CommonFunctions

solution = digitSum 10 . faculty $ 100