
---------------------------
-- Heuristic implemented by 'Map'. Used in A* algorithm
---------------------------
module Algorithms.Heuristic
    ( Heuristic
    -- * Lists
    , fromList
    -- * Properties
    , isAdmissible
    , isConsistency
    ) where

import           Algorithms.Graph (Graph, floydwarshall, vertices)
import qualified Data.Map         as M


-- | A Heuristic with keys @e@ and values @v@
type Heuristic e v = (e -> v)

{---------------------------
  Lists
----------------------------}
-- | Create Heuristic from list
fromList :: (Ord e,Num v,Ord v) => [(e, v)] -> Heuristic e v
fromList l k = M.findWithDefault 0 k (M.fromList l)


{----------------------------
  Properties
-----------------------------}
-- | Check heuristic on admissible for given graph
-- heruisitc is admissible if \forall A
-- h(A) \lt actual cost from A to Goal
isAdmissible :: (Integral e,Ord v)
             => Graph v e -> Heuristic v e -> v -> Bool
isAdmissible g h goal = all (`prop` goal) (filter (/= goal) (vertices g))
    where cost = floydwarshall g
          prop va vg = fromIntegral
                  (h va) <=
              cost va vg


-- | Check heuristic on consistency for given graph
-- heuristic is consistency if \forall A and C
-- h(A) - h(C) \lt actual cost from A to C
isConsistency :: (Integral e,Ord v)
              => Graph v e -> Heuristic v e -> Bool
isConsistency g h = checkProperty
        (vertices g)
        prop
    where cost = floydwarshall g
          prop va vg = fromIntegral
                  (h va -
                   h vg) <=
              cost va vg


{-------------------------------
  Internal functions
--------------------------------}
checkProperty :: [v] -> (v -> v -> Bool) -> Bool
checkProperty verts prop = all
        (\va ->
              all
                  (prop va)
                  verts)
        verts
