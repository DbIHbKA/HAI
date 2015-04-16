
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
    -- * Search Path
    , floydwarshall
    ) where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)


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
fromList :: (Ord v,Num e,Ord e,Integral e)
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
  Search Path
-----------------------------}
-- | Find shortest path from each pair of vertices
-- Floyd-Warshall algorithm
floydwarshall :: (Integral e,Ord v) => Graph v e -> v -> v -> Double
floydwarshall g vs ve = shortestPath
        (encode vs)
        (encode ve)
        n
    where vers = vertices g
          n = length vers
          encode v = fromMaybe 0 (M.lookup v g2)
          g2 = M.fromList
                  (zip vers [1 ..])
          eg = encodeGraph g
          edgeValue s e sg = case M.lookup s sg of
                  Nothing -> Nothing
                  Just edges -> case filter
                                         (\x ->
                                               e == fst x)
                                         edges of
                          [] -> Nothing
                          ((_v,_e):_) -> Just (fromIntegral _e)
          shortestPath i j 0 = fromMaybe infty (edgeValue i j eg)
          shortestPath i j k = min
                  (shortestPath
                       i
                       j
                       (k - 1))
                  (shortestPath
                       i
                       k
                       (k - 1) +
                   shortestPath
                       k
                       j
                       (k - 1))



{----------------------------
  Internal functions
-----------------------------}
-- | Encode graph with verices' labels are arbitrary symbol
-- to graph with vertices where labels are positive numbers
encodeGraph :: (Ord v) => Graph v e -> Graph Int e
encodeGraph g = M.map
        (map
             (\(v,e) ->
                   (encode v, e))) $
    M.mapKeys encode g
    where encode k1 = fromMaybe 0 (M.lookup k1 g2)
          g2 = M.fromList
                  (zip
                       (vertices g)
                       [1 ..])

-- | Infinity
infty :: Double
infty = read "Infinity"
