#define TESTS
#include <haskell>
import qualified Solid.Solver.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Solid.Solver.Spec.tests
  ]
