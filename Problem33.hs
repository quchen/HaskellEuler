{-
      Problem 33
            Discover all the fractions with an unorthodox cancelling method, and
            find the denominator of their product.

      Result
            100
            0.03 s
-}
module Problem33 (solution) where

import CommonFunctions
import Control.Monad
import Data.Ratio

solution = denominator . product' $ do
      b <- [1..9] -- 3 instances of this isn't enough to justify using sequence
      c <- [1..9]
      d <- [1..9]
      a <- [1..c-1] -- ensures numerator < denominator
      let frac  = (10 * a + b) % (10 * c + d)
      guard $ b == c && a % d == frac
              ||
              a == d && b % c == frac
      return frac