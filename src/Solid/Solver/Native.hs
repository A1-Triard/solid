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

data Spring = Spring
  { springStiffness :: Double
  , springLength :: Double
  , springBody1 :: Int
  , springPoint1 :: V3 Double
  , springBody2 :: Int
  , springPoint2 :: V3 Double
  }

data System = System
  { time :: !Double
  , timeDelta :: !Double
  , springs :: [Spring]
  , bodies :: Vector RigidBody
  }

data TorqueForce = TorqueForce
  { torque :: V3 Double
  , force :: V3 Double
  }

applySpring :: Vector RigidBody -> Vector TorqueForce -> Spring -> Vector TorqueForce
applySpring b forces spring =
  let i1 = springBody1 spring in
  let i2 = springBody2 spring in
  let b1 = fromMaybe (error "") $ b !? i1 in
  let b2 = fromMaybe (error "") $ b !? i2 in
  let p1 = L.rotate (bodyDirection b1) $ springPoint1 spring in
  let p2 = L.rotate (bodyDirection b2) $ springPoint2 spring in
  let dv = (bodyPosition b2 + p2) - (bodyPosition b1 + p1) in
  let sf = springStiffness spring *^ (dv - springLength spring *^ L.signorm dv) in
  V.imap (update sf i1 i2 p1 p2) forces
  where
    update sf i1 i2 p1 p2 i (TorqueForce t f)
      | i == i1 = TorqueForce (t + L.cross sf p1) (f + sf)
      | i == i2 = TorqueForce (t - L.cross sf p2) (f - sf)
      | otherwise = TorqueForce t f

advancePosition :: Double -> Bool -> Vector RigidBody -> Vector RigidBody
advancePosition d n s =
  V.map (\x -> x
    { bodyPosition = bodyPosition x + d *^ bodyVelocity x
    , bodyDirection = (if n then L.normalize else id) $ bodyDirection x + (0.5 * d) *^ (q $ L.inv44 (mB $ bodyDirection x) !* (mD !* bodyAngularVelocity x))
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

advanceVelocity :: Double -> [Spring] -> Vector RigidBody -> Vector RigidBody
advanceVelocity d s b =
  V.zipWith (\x (TorqueForce t f) -> x
    { bodyVelocity = bodyVelocity x + (d / bodyMass x) *^ f
    , bodyAngularVelocity = bodyAngularVelocity x + d *^ ((L.inv33 $ bodySpTensorOfInertia x) !* ((t ^/ bodyMass x) ^-^ (mW $ bodyAngularVelocity x) !* (bodySpTensorOfInertia x !* bodyAngularVelocity x)))
    }) b $ foldl (applySpring b) (V.replicate (V.length b) (TorqueForce 0.0 0.0)) s
  where
    mW (V3 x y z) =
      V3
        (V3 0.0 (-z) y)
        (V3 z 0.0 (-x))
        (V3 (-y) x 0.0)

advanceCore :: Bool -> System -> System
advanceCore n s =
  System (time s + timeDelta s) (timeDelta s) (springs s)
    $ advanceVelocity (timeDelta s) (springs s)
    $ advancePosition (timeDelta s) n
    $ bodies s

advance :: Double -> Bool -> System -> System
advance t n s =
  let steps = ceiling $ (t - time s) / (timeDelta s) in
  fromMaybe s $ listToMaybe $ drop steps $ iterate (advanceCore n) s

start :: Double -> [Spring] -> Vector RigidBody -> System
start dt s b = System 0.0 dt s $ advanceVelocity (dt / 2.0) s b
