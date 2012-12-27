{-
      Problem 29
            How many distinct values of a^b for 2 <= a,b <= 100 are there?

      Result
            9183
            .94 s
-}
module Problem29 (solution) where

import Control.Monad
import Data.List

solution = numDistinct powers
      where numDistinct = genericLength . group . sort
            powers = liftM2 (^) [2..100] [2..100]