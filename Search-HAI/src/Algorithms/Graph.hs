
------------------------
-- The implemetation of 'Graph' is based on 'Map'
------------------------
module Algorithms.Graph
    ( Graph
    -- * Lists
    , fromList
    -- * Methods
    , neighbours
    , neighboursWithEdges
    ) where

import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)

-- | A Graph with vertex @v@ and edges @e@
type Graph v e = Map v [(v, e)]

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

{-----------------------
  Methods
-------------------------}
-- | Return all neighbours of vertex
neighbours :: Ord v
           => v -> Graph v e -> [v]
neighbours v graph = map fst $
    fromMaybe [] (M.lookup v graph)

-- | Return all neighbours of vertex with weightes of his edges
neighboursWithEdges :: Ord v
                    => v -> Graph v e -> [(v, e)]
neighboursWithEdges v graph = fromMaybe [] (M.lookup v graph)
