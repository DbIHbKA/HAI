
module Algorithms.SearchHAI where

import           Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as HPSQ
import qualified Data.List    as L
import           Data.Map     (Map)
import qualified Data.Map     as M


type Graph v e = Map v [(v, e)]

-- | Create graph from list
fromList :: [(v, (v, e))] -> Graph v e
fromList = M.fromList

-- | Depth-First Tree Search
-- Find path in graph from start vertex to desired vertex
dfs :: v  -- ^ start vertex
    -> v  -- ^ desired vertex
    -> Graph v e -- ^ graph
    -> [v]
dfs s g graph = go
        s
        g
        (-2)
        (HPSQ.singleton
             s
             (-1)
             [s])
    where go current_vertex desired_vertex priority priority_queue = case HPSQ.findMin
                                                                              priority_queue of
                  Nothing -> []
                  Just (k,p,v) -> if L.elem desired_vertex v
                          then v
                          else go
                                   k
                                   desired_vertex
                                   (priority - 1)
                                   (foldr
                                        (\v_new pq ->
                                              HPSQ.insert
                                                  v_new
                                                  priority
                                                  (v ++
                                                   [v_new])
                                                  pq)
                                        priority_queue
                                        (neighbours current_vertex))
          neighbours v = map first $
              fromMaybe [] M.lookup v graph
