{-
      Problem 9
            Find the product a*b*c of the only Pythagorean triplet (a, b, c) for
            which a + b + c = 1000.

      Result
            31875000
            .58 s
-}
module Problem9 where

import Control.Monad

solution = head $ do
      c <- [1..]
      b <- [1..c-1]
      let a = 1000 - b - c
      guard $ a^2 + b^2 == c^2
      return $ a*b*c