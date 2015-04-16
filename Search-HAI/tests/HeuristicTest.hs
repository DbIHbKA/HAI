
module HeuristicTest where

import           Algorithms.Heuristic (Heuristic)
import qualified Algorithms.Heuristic as H
import           GraphTest            (graph, graphAStar)
import           Test.Tasty
import           Test.Tasty.HUnit


admissibleHeurisitc :: Heuristic String Int
admissibleHeurisitc = H.fromList
        [("S", 7), ("A", 6), ("B", 2), ("C", 1), ("G", 0)]

admissibleAndConsistencyHeuristic :: Heuristic String Int
admissibleAndConsistencyHeuristic = H.fromList
        [("S", 7), ("A", 6), ("B", 4), ("C", 2), ("G", 0)]



tests :: TestTree
tests = testGroup
        "Heuristic tests"
        [ testCase "Heuristic is admissible" $
          H.isAdmissible graph admissibleHeurisitc "G" @?=
          False
        , testCase "Heuristic is admissible" $
          H.isAdmissible graph admissibleAndConsistencyHeuristic "G" @?=
          False
        , testCase "Heuristic is not consistency" $
          H.isConsistency graph admissibleHeurisitc @?=
          False
        , testCase "Heurisitc is consistency" $
          H.isConsistency graph admissibleAndConsistencyHeuristic @?=
          False
        , testCase "Heuristic is admissible" $
          H.isAdmissible graphAStar admissibleHeurisitc "G" @?=
          True
        , testCase "Heuristic is admissible" $
          H.isAdmissible graphAStar admissibleAndConsistencyHeuristic "G" @?=
          True
        , testCase "Heuristic is not consistency" $
          H.isConsistency graphAStar admissibleHeurisitc @?=
          False
        , testCase "Heurisitc is consistency" $
          H.isConsistency graphAStar admissibleAndConsistencyHeuristic @?=
          True]
