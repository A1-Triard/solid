module Solid.Gui.Native where
#include <haskell>
import Paths_solid

data Vec3 = Vec3
  { vecX :: Double
  , vecY :: Double
  , vecZ :: Double
  }

{-# INLINE vecMult #-}
vecMult :: Double -> Vec3 -> Vec3
vecMult c (Vec3 x y z) = Vec3 (c * x) (c * y) (c * z)

{-# INLINE vecAdd #-}
vecAdd :: Vec3 -> Vec3 -> Vec3
vecAdd (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

{-# INLINE scalarProduct #-}
scalarProduct :: Vec3 -> Vec3 -> Double
scalarProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

{-# INLINE crossProduct #-}
crossProduct :: Vec3 -> Vec3 -> Vec3
crossProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

{-
{-# INLINE vecRotate #-}
vecRotate :: Vec3 -> Double -> Vec3 -> Vec3
vecRotate (Vec3 x y z) angle (Vec3 axe_x axe_y axe_z) =
-}

data RigidBody = RigidBody
  { bodyColor :: Vec3
  , bodyMass :: Double
  , bodyPosition :: Vec3
  , bodyVelocity :: Vec3
  , bodyDirection :: Vec3
  , bodyAngularVelocity :: Vec3
  }

bodyDraw :: RigidBody -> Render ()
bodyDraw b = do
  let v1 = bodyPosition b `vecAdd` bodyDirection b
  let v2 = bodyPosition b `vecAdd` Vec3 (vecY $ bodyDirection b) (-(vecX $ bodyDirection b)) (vecZ $ bodyDirection b)
  let v3 = bodyPosition b `vecAdd` Vec3 (-(vecX $ bodyDirection b)) (-(vecY $ bodyDirection b)) (vecZ $ bodyDirection b)
  let v4 = bodyPosition b `vecAdd` Vec3 (-(vecY $ bodyDirection b)) (vecX $ bodyDirection b) (vecZ $ bodyDirection b)
  setSourceRGB (vecX $ bodyColor b) (vecY $ bodyColor b) (vecZ $ bodyColor b)
  setLineWidth 1.0
  moveTo (vecX v1) (vecY v1)
  lineTo (vecX v3) (vecY v3)
  lineTo (vecX v4) (vecY v4)
  lineTo (vecX v2) (vecY v2)
  lineTo (vecX v1) (vecY v1)
  fill

test :: [RigidBody]
test =
  [ RigidBody
      (Vec3 1.0 1.0 1.0)
      1.0
      (Vec3 300.0 300.0 0.0)
      (Vec3 0.0 0.0 0.0)
      (Vec3 0.0 10.0 0.0)
      (Vec3 0.0 0.0 0.0)
  ]

solid :: IO ()
solid = do
  void initGUI
  b <- builderNew
  ui <- getDataFileName "ui.glade"
  builderAddFromFile b ui
  window <- builderGetObject b castToWindow ("applicationwindow" :: S.Text)
  void $ on window deleteEvent $ tryEvent $ lift mainQuit
  set window [windowTitle := ("Solid" :: S.Text)]
  d <- builderGetObject b castToDrawingArea ("drawingarea" :: S.Text)
  void $ on d draw $ do
    forM_ test bodyDraw
  widgetShowAll window
  mainGUI
