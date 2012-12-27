{-
      Problem 99
            Decyphering passwords by topological sorting

      Result
            73162890
            0.04 s

      Comment
            It is sufficient to compare the logs of the values, as log preserves
            order.
-}
module Problem79 (solution) where

import CommonFunctions
import Data.List
import Control.Applicative

solution = maybe (error msg) (implodeInt 10) $ topologicalSort passGraph
      where msg = "Cyclic graph in problem 79, algorithm wrong, panic!"

data Graph a = Graph [a] [(a, a)]
sources (Graph _ edges) = map fst edges
drains  (Graph _ edges) = map snd edges

-- Nothing if the graph is cyclic, otherwise the topologically sorted list
topologicalSort :: (Eq a) => Graph a -> Maybe [a]
topologicalSort g@(Graph v e)
      | null v            = Just [] -- No vertices left -> terminate
      | null onlyOutgoing = Nothing -- Cyclic graph: vertices left, but no outgoing
                                    -- edges -> vertex was already visited and
                                    -- removed, but there's still another instance
      | otherwise         = (x:) <$> topologicalSort (Graph v' e')
      where onlyOutgoing = filter (`notElem` drains g) v -- Vertices with only outgoing arrows
            x = head onlyOutgoing -- First vertex with only outgoing elements
                                  -- Note that onlyOutgoing is checked for null
                                  -- in the guards, so the head here is safe.
            v' = delete x v -- Remove (first instance of) vertex
                            -- (This ensures cyclic graphs fail, as another
                            -- version of the vertex still remains in that case)
            e' = filter ((/= x) . fst) e -- Remove all outgoing arrows
                                         -- associated to the vertex x

-- The code below converts the data into a graph of the format
-- Graph [vertex, vertex, ...] [(out, in), (out, in), ...]
passCodes = map explodeInt10 [
      319, 680, 180, 690, 129, 620, 762, 689, 762, 318, 368, 710, 720, 710,
      629, 168, 160, 689, 716, 731, 736, 729, 316, 729, 729, 710, 769, 290,
      719, 680, 318, 389, 162, 289, 162, 718, 729, 319, 790, 680, 890, 362,
      319, 760, 316, 729, 380, 319, 728, 716]
edges = passCodes >>= (\ ~[a,b,c] -> [(a,b), (b,c)])
vertices = nub . concat $ passCodes
passGraph = Graph vertices edges