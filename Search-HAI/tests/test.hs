
import qualified GraphTest     as GT
import qualified HeuristicTest as HT
import qualified SearchHAITest as SHAIT
import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "SearchAI tests" [SHAIT.tests, GT.tests, HT.tests]
