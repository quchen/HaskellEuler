{-
      Problem 56
            Considering natural numbers of the form a^b, where a, b < 100, what
            is the maximum digital sum?

      Result
            972
            1.8 s
-}
module Problem56 (solution) where

import CommonFunctions
import Control.Monad

solution = maximum' . map (digitSum 10) $ liftM2 (^) [1..100] [1..100]