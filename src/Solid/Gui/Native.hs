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

data System = System
  { time :: !Double
  , bodies :: [RigidBody]
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

systemDraw :: System -> Render ()
systemDraw s = do
  forM_ (bodies s) bodyDraw
  selectFontFace ("monospace" :: S.Text) FontSlantNormal FontWeightNormal
  setFontSize 14
  setSourceRGB 0.9 0.9 0.9
  moveTo 10.0 20.0
  showText $ ("Time = " ++) $ show $ time s

test :: [RigidBody]
test =
  [ RigidBody
      (Vec3 0.7 0.6 0.5)
      1.0
      (Vec3 300.0 300.0 0.0)
      (Vec3 1.0 0.0 0.0)
      (Vec3 20.0 20.0 0.0)
      (Vec3 0.0 0.0 1.0)
  ]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

advancePosition :: Double -> [RigidBody] -> [RigidBody]
advancePosition d s =
  map (\x -> x
    { bodyPosition = bodyPosition x `vecAdd` Vec3 (100.0 * d) 0.0 0.0
    , bodyDirection = bodyDirection x `vecAdd` (d `vecMult` (bodyAngularVelocity x `crossProduct` bodyDirection x))
    }) s

advanceCore :: Double -> System -> System
advanceCore d s = System (time s + d) (advancePosition d $ bodies s)

advance :: Double -> System -> System
advance !t s =
  let steps = ceiling $ (t - time s) / dt in
  fromMaybe s $ listToMaybe $ drop steps $ iterate (advanceCore dt) s

queueFrame :: IO Double -> DrawingArea -> System -> IO ()
queueFrame get_time d s = do
  draw_id <- on d draw $ systemDraw s
  void $ timeoutAdd (frame get_time d s draw_id) $ round (1000.0 / fps)

frame :: IO Double -> DrawingArea -> System -> ConnectId DrawingArea -> IO Bool
frame get_time d s draw_id = do
  t <- get_time
  signalDisconnect draw_id
  queueFrame get_time d $ advance t s
  widgetQueueDraw d
  return False

fps :: Double
fps = 100.0

dt :: Double
dt = 0.001

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
  t0 <- currentSeconds
  queueFrame ((subtract t0) <$> currentSeconds) d $ System 0.0 test
  widgetShowAll window
  mainGUI
