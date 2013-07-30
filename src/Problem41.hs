{-
      Problem 41
            Find the largest pandigital prime

      Result
            0.01 s

      Comment
            "Why does the search start with a 7-pandigital and not one of rank 9?
            All n-pandigitals of a certain order have the same digit sum:
                  n = 9 => digit sum 45
                  n = 8 => digit sum 36
                  n = 7 => digit sum 28
                  ...
            This shows that n = 8,9 cannot be pandigital primes, as their digit
            sum is divisible by 3. It is also known that there is a pandigital
            prime because the problem gives 2143 as an example, which is a
            4-pandigital. The rest of the algorithm is brute force.
-}
module Problem41 (solution) where

import Data.Numbers.Primes
import qualified Data.Permute as P
import Data.Maybe
import CommonFunctions

solution = fromIntegral . prevPanPrime $ P.listPermute 7 [6,5..0]
      where prevPanPrime p
                  | isPrime n = n
                  | otherwise = prevPanPrime . fromMaybe prunedP $ P.prev p
                  where
                        n = getNumber p
                        -- When the algorithm reaches the smallest n-pandigital,
                        -- this generates the permutation for the largest
                        -- (n-1) pandigital. Example: p = 123 => prunedP = 21
                        -- Note that this is not actually needed here, since
                        -- there is a 7-digit pandigital, but you can't know
                        -- that in advance :-)
                        prunedP = P.listPermute (length ipe) $ reverse ipe
                        ipe = init . P.elems $ p

                        -- Extract current number out of a permutation
                        getNumber = implodeInt 10 . map (+1) . P.elems