{-# LANGUAGE TupleSections #-}

module Main where

import ProjectEuler
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import Control.Parallel
import Control.Parallel.Strategies
import Text.Printf

-- | Returns 'Just (number, result)' for solved problems, otherwise 'Nothing'
solveEuler :: Int -> Maybe (Int, Integer)
solveEuler n = fmap (n,) $ euler n

main = do
      let results = mapMaybe solveEuler [1..10^2] `using` parList rseq
      putStrLn $ "Project Euler in Haskell"
      putStrLn $ "Total problems solved: " ++ show (length results) -- Hooray for laziness!
      putStrLn $ "Problem  Solution"
      forM_ results $ putStrLn . pretty

-- | Formats a tuple (problem_number, solution) nicely.
pretty :: (Int, Integer) -> String
pretty (p, s) = printf "%7d  %d" p s -- 7 = length "Problem"