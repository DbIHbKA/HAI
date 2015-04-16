
module GraphTest where

import           Algorithms.Graph (Graph)
import qualified Algorithms.Graph as G
import           Test.Tasty
import           Test.Tasty.HUnit



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


tests :: TestTree
tests = testGroup "Graph tests" [floydwarchallUnitTests]


floydwarchallUnitTests = testGroup
        "Floyd-Warshall testing"
        [ testCase "Path from S to B" $
          G.floydwarshall graph "S" "B" @?=
          2.0
        , testCase "Path from S to A" $
          G.floydwarshall graph "S" "A" @?=
          1.0
        , testCase "Path from A to B" $
          G.floydwarshall graph "A" "B" @?=
          1.0
        , testCase "Path from S to B in graphAStar" $
          G.floydwarshall graphAStar "S" "B" @?=
          3.0
        , testCase "Path from A to G in graphAStar" $
          G.floydwarshall graphAStar "A" "G" @?=
          7.0]
