module Data.MatrixSpace.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.MatrixSpace

tests :: Test
tests = TestList
  [ TestCase qrotateTest
  ]

assertDouble :: String -> Double -> Double -> Assertion
assertDouble msg a b = assertBool msg (abs(a - b) <= 1e-10)

assertMat :: (KnownNat r, KnownNat c) => String -> Mat r c Double -> Mat r c Double -> Assertion
assertMat msg (Mat a) (Mat b) = sequence_ $ elementwise (assertDouble msg) a b

qrotateTest :: Assertion
qrotateTest = do
  let r = qrotation ((pi :: Double) / 2.0) (vec3 0.0 0.0 1.0)
  assertMat "" (vec3 0.0 1.0 0.0) $ qrotate r (vec3 1.0 0.0 0.0)
