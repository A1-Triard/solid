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
  , springBLength :: Double
  , springBodyIndex1 :: Int
  , springBodyPoint1 :: V3 Double
  , springBodyIndex2 :: Int
  , springBodyPoint2 :: V3 Double
  , springDelta :: Double
  , springDirection :: V3 Double
  }

data System = System
  { time :: !Double
  , timeDelta :: !Double
  , springs :: Vector Spring
  , bodies :: Vector RigidBody
  }

data TorqueForce = TorqueForce
  { torque :: V3 Double
  , force :: V3 Double
  }

bodyKineticEnergy :: RigidBody -> Double
bodyKineticEnergy b =
  0.5 * bodyMass b *
    ( L.quadrance (bodyVelocity b)
    + (bodySpTensorOfInertia b !* bodyAngularVelocity b) `L.dot` bodyAngularVelocity b
    )

springPotentialEnergy :: Spring -> Double
springPotentialEnergy s =
  let d = springDelta s in
  if d <= 0.0 then 0.0 else 0.5 * springStiffness s * d * d

kineticEnergy :: Vector RigidBody -> Double
kineticEnergy = V.foldl (\s -> (s +) . bodyKineticEnergy) 0.0

potentialEnergy :: Vector Spring -> Double
potentialEnergy = V.foldl (\s -> (s +) . springPotentialEnergy) 0.0

updateSpring :: Vector RigidBody -> Spring -> Spring
updateSpring b spring =
  let i1 = springBodyIndex1 spring in
  let i2 = springBodyIndex2 spring in
  let b1 = fromMaybe (error "") $ b !? i1 in
  let b2 = fromMaybe (error "") $ b !? i2 in
  let p1 = L.rotate (bodyDirection b1) $ springBodyPoint1 spring in
  let p2 = L.rotate (bodyDirection b2) $ springBodyPoint2 spring in
  let d = (bodyPosition b2 + p2) - (bodyPosition b1 + p1) in
  spring { springDelta = L.norm d - springBLength spring, springDirection = L.signorm d }

applySpring :: Vector TorqueForce -> Spring -> Vector TorqueForce
applySpring forces spring =
  let i1 = springBodyIndex1 spring in
  let i2 = springBodyIndex2 spring in
  let d = springDelta spring in
  let sf = if d <= 0.0 then 0.0 else (springStiffness spring * d) *^ springDirection spring in
  V.imap (update sf i1 i2) forces
  where
    update sf i1 i2 i (TorqueForce t f)
      | i == i1 = TorqueForce (t + L.cross sf (springBodyPoint1 spring)) (f + sf)
      | i == i2 = TorqueForce (t - L.cross sf (springBodyPoint2 spring)) (f - sf)
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

advanceVelocity :: Double -> Vector TorqueForce -> Vector RigidBody -> Vector RigidBody
advanceVelocity d fs b =
  V.zipWith (\x (TorqueForce t f) -> x
    { bodyVelocity = bodyVelocity x + (d / bodyMass x) *^ f
    , bodyAngularVelocity = bodyAngularVelocity x + d *^ ((L.inv33 $ bodySpTensorOfInertia x) !* ((t ^/ bodyMass x) ^-^ (mW $ bodyAngularVelocity x) !* (bodySpTensorOfInertia x !* bodyAngularVelocity x)))
    }) b fs
  where
    mW (V3 x y z) =
      V3
        (V3 0.0 (-z) y)
        (V3 z 0.0 (-x))
        (V3 (-y) x 0.0)

advanceStep :: Bool -> (Int -> Vector Spring -> Vector TorqueForce) -> System -> System
advanceStep n forces s =
  let b1 = advancePosition (timeDelta s) n $ bodies s in
  let s1 = V.map (updateSpring b1) $ springs s in
  let f = forces (V.length b1) s1 in
  let b2 = advanceVelocity (timeDelta s) f b1 in
  System (time s + timeDelta s) (timeDelta s) s1 b2

advanceCore :: Double -> Bool -> (Int -> Vector Spring -> Vector TorqueForce) -> System -> System
advanceCore t n forces s =
  let steps = ceiling $ (t - time s) / (timeDelta s) in
  fromMaybe s $ listToMaybe $ drop steps $ iterate (advanceStep n forces) s

advance :: Double -> Bool -> System -> System
advance t n = advanceCore t n springForces

springForces :: Int -> Vector Spring -> Vector TorqueForce
springForces m = V.foldl applySpring (V.replicate m (TorqueForce 0.0 0.0))

startCore :: Double -> (Int -> Vector Spring -> Vector TorqueForce) -> Vector Spring -> Vector RigidBody -> System
startCore dt forces s b =
  let s1 = V.map (updateSpring b) s in
  let b1 = advanceVelocity (dt / 2.0) (forces (V.length b) s1) b in
  System 0.0 dt s1 b1

start :: Double -> Vector Spring -> Vector RigidBody -> System
start dt = startCore dt springForces
