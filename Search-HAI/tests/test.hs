
import           Test.Tasty
import           Test.Tasty.HUnit

import           Algorithms.Graph     (Graph)
import qualified Algorithms.Graph     as G
import           Algorithms.Heuristic (Heuristic)
import qualified Algorithms.Heuristic as H
import           Algorithms.SearchHAI


graph :: Graph String Int
graph = G.fromList
        [ ("S", ("A", 1))
        , ("S", ("B", 2))
        , ("A", ("B", 1))
        , ("A", ("C", 1))
        , ("B", ("C", 1))
        , ("C", ("G", 1))]


graphAStar :: Graph String Int
graphAStar = G.fromList
        [ ("S", ("A", 1))
        , ("S", ("B", 4))
        , ("A", ("B", 2))
        , ("A", ("C", 5))
        , ("A", ("G", 12))
        , ("B", ("C", 2))
        , ("C", ("G", 3))]


admissibleHeurisitc :: Heuristic String Int
admissibleHeurisitc = H.fromList
        [("S", 7), ("A", 6), ("B", 2), ("C", 1), ("G", 0)]

admissibleAndConsistencyHeuristic :: Heuristic String Int
admissibleAndConsistencyHeuristic = H.fromList
        [("S", 7), ("A", 6), ("B", 4), ("C", 2), ("G", 0)]

main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [dfsUnitTests, bfsUnitTests, aStarUnitTests]


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
