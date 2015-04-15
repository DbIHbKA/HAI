
{-# LANGUAGE OverloadedStrings #-}

module Algorithms.SearchHAI
       ( bfs
       , dfs
       , aStarTS
       , aStar
       ) where

import           Algorithms.Graph     (Graph, neighbours, neighboursWithEdges)
import           Algorithms.Heuristic (Heuristic)
import           Data.Hashable        (Hashable)
import qualified Data.HashPSQ         as HPSQ
import qualified Data.List            as L
import qualified Data.Set             as S


-- | Depth-First Tree Search
-- Finds path between two vertices in a graph
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
-- Finds shortest path between two vertices in a graph
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


-- | A* Tree search
-- Finds the least weight path between two vertices in a graph
-- if there is no path return empty path
aStarTS :: (Ord v,Hashable v,Ord e,Num e)
        => v -> v -> Graph v e -> Heuristic v e -> [v]
aStarTS s g graph heuristic = go
        g
        (HPSQ.singleton
             [s]
             (heuristic s)
             s)
    where go desired_vertex priority_queue = case HPSQ.minView priority_queue of
                  Nothing -> []
                  Just (v,p,k,pq) -> if L.elem desired_vertex v
                          then v
                          else go
                                   desired_vertex
                                   (addStarNeighbours pq v p k graph heuristic)


-- | A* Graph search
-- Modification of A* tree search.  We don't use evaluated vertices.
aStar :: (Ord v,Hashable v,Ord e,Num e,Show v,Show e)
      => v -> v -> Graph v e -> Heuristic v e -> [v]
aStar s g graph heuristic = go
        g
        (HPSQ.singleton
             [s]
             (heuristic s)
             s)
        S.empty
    where go desired_vertex priority_queue closedSet = case HPSQ.minView
                                                                priority_queue of
                  Nothing -> []
                  Just (v,p,k,pq)
                      | L.elem desired_vertex v ->
                          v
                      | S.member k closedSet ->
                          go desired_vertex pq closedSet
                      | otherwise ->
                          go
                              desired_vertex
                              (addStarNeighbours pq v p k graph heuristic)
                              (S.insert k closedSet)

-- Internal functions

-- | Add all neighbors of given vertex with heuristic priority to Priority Queue
addStarNeighbours :: (Ord v,Hashable v,Ord e,Num e)
                  => HPSQ.HashPSQ [v] e v  -- ^ Priority queue
                  -> [v]  -- ^ Current path
                  -> e  -- ^ priority
                  -> v  -- ^ current vertex
                  -> Graph v e  -- ^ graph
                  -> Heuristic v e  -- ^ heuristic
                  -> HPSQ.HashPSQ [v] e v
addStarNeighbours pq cp p cv g h = foldr
        (\(v_new,e_new) _pq ->
              HPSQ.insert
                  (cp ++
                   [v_new])
                  (p - h cv + e_new + h v_new)
                  v_new
                  _pq)
        pq
        (neighboursWithEdges cv g)

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
