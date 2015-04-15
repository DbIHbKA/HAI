{-# LANGUAGE OverloadedStrings #-}

module Algorithms.SearchHAI
       (bfs, dfs) where

import           Algorithms.DataTypes (Graph, neighbours)
import           Data.Hashable        (Hashable)
import qualified Data.HashPSQ         as HPSQ
import qualified Data.List            as L


-- | Depth-First Tree Search
-- Find path in graph from start vertex to desired vertex
-- If there is no path return empty path
dfs :: (Ord v,Hashable v)
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
    where go desired_vertex priority_queue = case HPSQ.minView priority_queue of
                  Nothing -> []
                  Just (v,p,k,pq) -> if L.elem desired_vertex v
                          then v
                          else go
                                   desired_vertex
                                   (addNeighbours
                                        pq
                                        k
                                        (p - 1)
                                        v
                                        graph)


-- | Breadth-First Tree Search
-- Find shortest path in graph between two vertexes
-- if there is no path return empty path
bfs :: (Ord v,Hashable v)
    => v  -- ^ start vertex
    -> v  -- ^ desired vertex
    -> Graph v e
    -> [v]
bfs s g graph = go
        g
        (HPSQ.singleton
             [s]
             1
             s)
    where go desired_vertex priority_queue = case HPSQ.minView priority_queue of
                  Nothing -> []
                  Just (v,p,k,pq) -> if L.elem desired_vertex v
                          then v
                          else go
                                   desired_vertex
                                   (addNeighbours
                                        pq
                                        k
                                        (p + 1)
                                        v
                                        graph)


-- Internal functions

-- | Add all neighbors of given vertex to Priority Queue
addNeighbours :: (Ord v,Hashable v)
              => HPSQ.HashPSQ [v] Int v
              -> v
              -> Int
              -> [v]
              -> Graph v e
              -> HPSQ.HashPSQ [v] Int v
addNeighbours pq cv p v graph = foldr
        (\v_new _pq ->
              HPSQ.insert
                  (v ++
                   [v_new])
                  p
                  v_new
                  _pq)
        pq
        (neighbours cv graph)
