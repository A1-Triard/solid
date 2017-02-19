#define TESTS
#include <haskell>
import qualified Data.MatrixSpace.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.MatrixSpace.Spec.tests
  ]
