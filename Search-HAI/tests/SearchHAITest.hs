
module SearchHAITest where

import Test.Tasty
import           Test.Tasty.HUnit
import           Algorithms.SearchHAI
import           GraphTest
import           HeuristicTest


tests :: TestTree
tests = testGroup "SearchHAI tests" [dfsUnitTests, bfsUnitTests, aStarUnitTests]


dfsUnitTests = testGroup
        "dfs Unit tests"
        [ testCase "Find path with dfs from S to G" $
          dfs "S" "G" graph @?= ["S", "A", "B", "C", "G"]
        , testCase "Find no path with dfs" $
          dfs "S" "Q" graph @?= []]


bfsUnitTests = testGroup
        "bfs Unit tests"
        [ testCase "Find path with bfs from S to G" $
          bfs "S" "G" graph @?=
          ["S", "A", "C", "G"]
        , testCase "Find no path with bfs" $
          bfs "S" "Q" graph @?= []]

aStarUnitTests = testGroup
        "A* Unit tests"
        [ testCase "Find path with A* Tree Search from S to G" $
          aStarTS "S" "G" graphAStar admissibleHeurisitc @?= ["S", "A", "B", "C", "G"]
        , testCase "Find path with A* from S to G" $
          aStar "S" "G" graphAStar admissibleHeurisitc @?= ["S", "A", "C", "G"]
        , testCase "Find path with A* from S to G with admissible and consistency heuristic" $
          aStar "S" "G" graphAStar admissibleAndConsistencyHeuristic @?= ["S", "A", "B", "C", "G"]
        ]
