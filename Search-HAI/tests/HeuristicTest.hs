
module HeuristicTest where


import           Algorithms.Heuristic (Heuristic)
import qualified Algorithms.Heuristic as H


admissibleHeurisitc :: Heuristic String Int
admissibleHeurisitc = H.fromList
        [("S", 7), ("A", 6), ("B", 2), ("C", 1), ("G", 0)]

admissibleAndConsistencyHeuristic :: Heuristic String Int
admissibleAndConsistencyHeuristic = H.fromList
        [("S", 7), ("A", 6), ("B", 4), ("C", 2), ("G", 0)]

