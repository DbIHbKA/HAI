
import           Test.Tasty
import           Test.Tasty.HUnit

import           Algorithms.DataTypes (Graph, fromList)
import           Algorithms.SearchHAI


graph :: Graph String Int
graph = fromList [("S", ("A", 1))
                 ,("S", ("B", 2))
                 ,("A", ("B", 1))
                 ,("A", ("C", 1))
                 ,("B", ("C", 1))
                 ,("C", ("G", 1))]

main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [dfsUnitTests]


dfsUnitTests = testGroup
        "dfs Unit tests"
        [ testCase "Find path with dfs from S to G" $
          dfs "S" "G" graph @?= ["S", "A", "B", "C", "G"]
        , testCase "Find no path with dfs" $
          dfs "S" "Q" graph @?= []]


bfsUnitTests = testGroup
             "bfs Unit tests" [ testCase "Find path with bfs from S to G" $ bfs "S" "G" graph @?= ["S", "A", "C", "G"]]
