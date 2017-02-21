module Solid.Gui.Native where

#include <haskell>
import Paths_solid

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
  , bodies :: [RigidBody]
  }

bodyDraw :: RigidBody -> Render ()
bodyDraw b = do
  let v = L.rotate (bodyDirection b) (V3 20.0 0.0 0.0)
  let v1 = bodyPosition b + v
  let v2 = bodyPosition b + V3 (v ^._y) (-(v ^._x)) (v ^._z)
  let v3 = bodyPosition b + V3 (-(v ^._x)) (-(v ^._y)) (v ^._z)
  let v4 = bodyPosition b + V3 (-(v ^._y)) (v ^._x) (v ^._z)
  setSourceRGB (bodyColor b ^._x) (bodyColor b ^._y) (bodyColor b ^._z)
  setLineWidth 1.0
  moveTo (v1 ^._x) (-(v1 ^._y))
  lineTo (v3 ^._x) (-(v3 ^._y))
  lineTo (v4 ^._x) (-(v4 ^._y))
  lineTo (v2 ^._x) (-(v2 ^._y))
  lineTo (v1 ^._x) (-(v1 ^._y))
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
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 300.0 (-300.0) 0.0)
      (V3 100.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 1.0 0.0 (-1.0))
  ]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

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

advanceCore :: Double -> System -> System
advanceCore d s = System (time s + d) (advanceVelocity d $ advancePosition d $ bodies s)

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
  queueFrame ((subtract t0) <$> currentSeconds) d $ System 0.0 $ advanceVelocity (dt / 2.0) test
  widgetShowAll window
  mainGUI
