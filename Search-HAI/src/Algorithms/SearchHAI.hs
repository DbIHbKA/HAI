{-# LANGUAGE OverloadedStrings #-}

module Algorithms.SearchHAI where

import           Data.Hashable (Hashable)
import qualified Data.HashPSQ  as HPSQ
import qualified Data.List     as L
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

-- | Depth-Fst Tree Search
-- Find path in graph from start vertex to desired vertex
dfs :: (Ord v,Hashable v, Show v)
    => v  -- ^ start vertex
    -> v  -- ^ desired vertex
    -> Graph v e -- ^ graph
    -> [v]
dfs s g graph = go
        g
        (HPSQ.singleton
             [s]
             (-1)
             s)
    where go desired_vertex priority_queue = case HPSQ.findMin priority_queue of
                  Nothing -> []
                  Just (v,p,k) -> if L.elem desired_vertex v
                          then v
                          else go
                                   desired_vertex
                                   (add_neighbours
                                        priority_queue
                                        k
                                        (p - 1)
                                        v)
          neighbours v = map fst $
              fromMaybe [] (M.lookup v graph)
          add_neighbours pq cv p v = foldr
                  (\v_new _pq ->
                        HPSQ.insert
                            (v ++
                             [v_new])
                            p
                            v_new
                            _pq)
                  pq
                  (neighbours cv)
