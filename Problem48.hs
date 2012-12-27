{-
      Problem 48
            Find the last ten digits of 'sum n^n'.

      Result
            9110846700
            .03 s

      Comment
            The heart of this is using an efficient power implementation. The
            one provided by the (self-written) Power library computes it in
            O(log(n)), but the naive usage still produces very large inter-
            mediate numbers. However, by truncating every intermediate result
            of the multiplications required to calculate the powers gets rid
            of this issue, and the algorithm scales well.
            (Profiling shows that the naive implementation, shown below the
            following code, outperforms this code by a small margin. However,
            if the maximum n is increased to 10^4, it already loses by an order
            of magnitude, and 10^5 took so long I aborted it. The truncation-
            based version still finished quickly.)
-}
module Problem48 (solution) where

import Power
import CommonFunctions

solution = last10 $ sum' [n ^. n | n <- [1..1000]]
      where last10 x = x `rem` 10^10
            a ^. b = Power.power (\x y -> last10 $! x * y) b a