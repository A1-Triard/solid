module Solid.Solver.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Solid.Solver

tests :: Test
tests = TestList
  [ TestCase constantMotionTest
  ]

data TestPair a = TestPair String a a

instance Assertable (TestPair Double) where
  assert (TestPair msg a b) = assertBool (msg ++ ": " ++ show a ++ " != " ++ show b) (abs(a - b) <= 1e-5)

instance (Foldable f, Assertable (TestPair a)) => Assertable (TestPair (f a)) where
  assert (TestPair msg a b) = forM_ (zip (toList a) (toList b)) $ \(x, y) -> assert $ TestPair msg x y

instance Assertable (TestPair RigidBody) where
  assert (TestPair msg a b) = do
    assert $ TestPair (msg ++ ": " ++ "color") (bodyColor a) (bodyColor b)
    assert $ TestPair (msg ++ ": " ++ "mass") (bodyMass a) (bodyMass b)
    assert $ TestPair (msg ++ ": " ++ "tensor") (bodySpTensorOfInertia a) (bodySpTensorOfInertia b)
    assert $ TestPair (msg ++ ": " ++ "pos") (bodyPosition a) (bodyPosition b)
    assert $ TestPair (msg ++ ": " ++ "velocity") (bodyVelocity a) (bodyVelocity b)
    assert $ TestPair (msg ++ ": " ++ "direction") (bodyDirection a) (bodyDirection b)
    assert $ TestPair (msg ++ ": " ++ "avel") (bodyAngularVelocity a) (bodyAngularVelocity b)

test1 :: Vector RigidBody
test1 = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 300.0 (-300.0) 0.0)
      (V3 100.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 pi)
  ]

test1Result :: Vector RigidBody
test1Result = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 1300.0 (-300.0) 0.0)
      (V3 100.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (10.5 * pi))
      (V3 0.0 0.0 pi)
  ]

constantMotionTest :: Assertion
constantMotionTest = do
  assert $ TestPair "" test1Result $ bodies $ advance 10.0 True $ start 0.0001 V.empty test1
