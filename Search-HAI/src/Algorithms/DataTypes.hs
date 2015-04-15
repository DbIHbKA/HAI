
module Algorithms.DataTypes
    ( Graph
    , fromList
    , neighbours
    ) where

import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)


type Graph v e = Map v [(v, e)]

-- | Create graph from list
fromList :: Ord v
         => [(v,(v,e))] -> Graph v e
fromList = foldr
        (\(k,(v,w)) m ->
              M.insertWith
                  (++)
                  k
                  [(v, w)]
                  m)
        M.empty

-- | Return all neighbours of vertex
neighbours :: Ord v
           => v -> Graph v e -> [v]
neighbours v graph = map fst $
    fromMaybe [] (M.lookup v graph)
