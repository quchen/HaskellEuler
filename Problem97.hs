{-
      Problem 97
            Find the last 10 digits of the prime 28433 * 2^7830457 + 1.

      Result
            8739992577
            0.01 s

      Comment
            The key is efficient calculation of the power. Using the (self-
            written) Power package, the number of multiplications scales like
            log(exponent). The multiplications at each step have to be carried
            out for the last 10 digits, as the other digits don't influence the
            last digits in subsequent calculations.
-}
module Problem97 (solution) where

import Power

solution = last10 $ 28433 * 2^.7830457 + 1

last10 x = x `rem` 10^10

infixr 8 ^.
b ^. e = power (\x y -> last10 $! x * y) e b