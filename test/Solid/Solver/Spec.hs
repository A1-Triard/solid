--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Solid.Solver.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Solid.Solver

tests :: Test
tests = TestList
  [ TestCase constantMotionTest
  , TestCase uniformlyAcceleratedRotationTest
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

test2 :: Vector RigidBody
test2 = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 1300.0 (-300.0) 0.0)
      (V3 100.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (10.5 * pi))
      (V3 0.0 0.0 pi)
  ]

test3 :: Vector RigidBody
test3 = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 300.0 (-300.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 0.0)
  ]

test4 :: Vector RigidBody
test4 = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 300.0 (-300.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) pi)
      (V3 0.0 0.0 (pi / 10.0))
  ]

constantMotionTest :: Assertion
constantMotionTest = do
  assert $ TestPair "" test2 $ bodies $ advance 10.0 True $ start 0.0001 V.empty test1

uniformlyAcceleratedRotationTest :: Assertion
uniformlyAcceleratedRotationTest = do
  let forces = \_ _ -> V.fromList [TorqueForce (V3 0.0 0.0 (pi / 100.0)) (V3 0.0 0.0 0.0)]
  assert $ TestPair "" test4 $ bodies $ advanceCore 10.0 True forces $ startCore 0.0001 forces V.empty test3
