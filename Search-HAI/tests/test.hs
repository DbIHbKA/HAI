
import           Test.Tasty
import           Test.Tasty.HUnit

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
        [ testsCase "Find path with dfs fro S to G" $
          dfs "S" "G" graph ==
          ["S", "A", "B", "C", "G"]]
