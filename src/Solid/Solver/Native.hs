module Solid.Solver.Native where

#include <haskell>

data RigidBody = RigidBody
  { bodyColor :: V3 Double
  , bodyMass :: Double
  , bodySpTensorOfInertia :: M33 Double
  , bodyPosition :: V3 Double
  , bodyVelocity :: V3 Double
  , bodyDirection :: Quaternion Double
  , bodyAngularVelocity :: V3 Double
  }

data System = System
  { time :: !Double
  , timeDelta :: !Double
  , bodies :: [RigidBody]
  }

advancePosition :: Double -> [RigidBody] -> [RigidBody]
advancePosition d s =
  map (\x -> x
    { bodyPosition = bodyPosition x + d *^ bodyVelocity x
    , bodyDirection = bodyDirection x + (0.5 * d) *^ (q $ L.inv44 (mB $ bodyDirection x) !* (mD !* bodyAngularVelocity x))
    }) s
  where
    mD = V4 (V3 0.0 0.0 0.0) (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
    mB (Quaternion v0 (V3 v1 v2 v3)) =
      V4
        (V4 v0 v1 v2 v3)
        (V4 (-v1) v0 (-v3) v2)
        (V4 (-v2) v3 v0 (-v1))
        (V4 (-v3) (-v2) v1 v0)
    q (V4 v0 v1 v2 v3) = Quaternion v0 (V3 v1 v2 v3)

advanceVelocity :: Double -> [RigidBody] -> [RigidBody]
advanceVelocity d s =
  map (\x -> x
    { bodyVelocity = bodyVelocity x + (d / bodyMass x) *^ force
    , bodyAngularVelocity = bodyAngularVelocity x + d *^ ((L.inv33 $ bodySpTensorOfInertia x) !* ((torque ^/ bodyMass x) ^-^ (mW $ bodyAngularVelocity x) !* (bodySpTensorOfInertia x !* bodyAngularVelocity x)))
    }) s
  where
    mW (V3 x y z) =
      V3
        (V3 0.0 (-z) y)
        (V3 z 0.0 (-x))
        (V3 (-y) x 0.0)
    force = V3 0.0 (-10.0) 0.0
    torque = V3 0.0 0.0 0.3

advanceCore :: System -> System
advanceCore s = System (time s + timeDelta s) (timeDelta s) $ advanceVelocity (timeDelta s) $ advancePosition (timeDelta s) $ bodies s

advance :: Double -> System -> System
advance t s =
  let steps = ceiling $ (t - time s) / (timeDelta s) in
  fromMaybe s $ listToMaybe $ drop steps $ iterate advanceCore s

start :: Double -> [RigidBody] -> System
start dt b = System 0.0 dt $ advanceVelocity (dt / 2.0) b
