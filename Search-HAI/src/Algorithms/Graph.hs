
------------------------
-- The implemetation of 'Graph' is based on 'Map'
------------------------
module Algorithms.Graph
    ( Graph
    -- * Conversion
    , vertices
    -- * Lists
    , fromList
    -- * Search Neighbours
    , neighbours
    , neighboursWithEdges
    ) where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as S

-- | A Graph with vertex @v@ and edges @e@
type Graph v e = Map v [(v, e)]


{------------------------
  Conversion
-------------------------}
-- | Return all vertices of the graph
vertices :: Graph v e -> [v]
vertices = M.keys


{------------------------
  Lists
-------------------------}
-- | Create graph from list
fromList :: (Ord v,Num e,Ord e)
         => [(v,(v,e))] -> Graph v e
fromList = foldr
        (\(k,(v,w)) m ->
              M.insertWith
                  (++)
                  k
                  [(v, w)]
                  m)
        M.empty

{-------------------------
  Search Neighbours
--------------------------}
-- | Return all neighbours of vertex
neighbours :: Ord v
           => v -> Graph v e -> [v]
neighbours v graph = map fst $
    fromMaybe [] (M.lookup v graph)

-- | Return all neighbours of vertex with weightes of his edges
neighboursWithEdges :: Ord v
                    => v -> Graph v e -> [(v, e)]
neighboursWithEdges v graph = fromMaybe [] (M.lookup v graph)

{----------------------------
  Sort
-----------------------------}
topsort :: Graph v e -> Graph v e
topsort g = topsort' (vertices g) [] [] g


topsort' (white:whites) greys black graph = go
        whites
        (white : greys)
        black
        white
    where go ws [] bs c = case neighbours c graph of
                  [] -> topsort' ws [] bs graph
                  (c':_) -> go ws [c] bs c'
          go ws (g:gs) bs c = case neighbours c graph of
                  [] -> go ws gs (c : bs) g
                  cs -> case notInGreyC
                                 cs
                                 (g : gs) of
                          Nothing -> go ws gs (c : bs) g
                          Just c' -> go ws (g : gs) bs c'
          notInGreyC = undefined
