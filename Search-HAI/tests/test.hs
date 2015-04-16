
import qualified SearchHAITest as SHAIT
import qualified GraphTest as GT
import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "SearchAI tests" [SHAIT.tests, GT.tests]
