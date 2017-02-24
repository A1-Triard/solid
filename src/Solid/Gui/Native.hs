module Solid.Gui.Native where

#include <haskell>
import Solid.Solver
import Paths_solid

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
  showText $ "Time = " ++ (show $ time s)
  moveTo 10.0 40.0
  showText $ "Kinetic Energy = " ++ (show $ kineticEnergy $ bodies s)

testBodies :: Vector RigidBody
testBodies = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (V3 (V3 10.0 0.0 0.0) (V3 0.0 10.0 0.0) (V3 0.0 0.0 10.0))
      (V3 300.0 (-300.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 0.0)
  , RigidBody
      (V3 0.5 0.6 0.7)
      1.0
      (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))
      (V3 600.0 (-600.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 0.0)
  ]

testSprings :: [Spring]
testSprings =
  [ Spring
      0.2
      5.0
      0 (V3 50.0 0.0 10.0)
      1 (V3 0.0 (-50.0) (-10.0))
  ]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

data UI = UI
  { timer :: IO Double
  , canvas :: DrawingArea
  , normalizeQ :: CheckMenuItem
  }

queueFrame :: UI -> System -> IO ()
queueFrame ui s = do
  draw_id <- on (canvas ui) draw $ systemDraw s
  void $ timeoutAdd (frame ui s draw_id) $ round (1000.0 / fps)

frame :: UI -> System -> ConnectId DrawingArea -> IO Bool
frame ui s draw_id = do
  t <- timer ui
  nq <- checkMenuItemGetActive $ normalizeQ ui
  signalDisconnect draw_id
  queueFrame ui $ advance t nq s
  widgetQueueDraw $ canvas ui
  return False

fps :: Double
fps = 100.0

dt :: Double
dt = 0.0001

solid :: IO ()
solid = do
  void initGUI
  b <- builderNew
  builderAddFromFile b =<< getDataFileName "ui.glade"
  window <- builderGetObject b castToWindow ("applicationwindow" :: S.Text)
  void $ on window deleteEvent $ tryEvent $ lift mainQuit
  d <- builderGetObject b castToDrawingArea ("drawingarea" :: S.Text)
  quit <- builderGetObject b castToMenuItem ("quititem" :: S.Text)
  void $ on quit menuItemActivated mainQuit
  n <- builderGetObject b castToCheckMenuItem ("normalize" :: S.Text)
  t0 <- currentSeconds
  let
    ui = UI
      { timer = (subtract t0) <$> currentSeconds
      , canvas = d
      , normalizeQ = n
      }
  queueFrame ui $ start dt testSprings testBodies
  widgetShowAll window
  mainGUI
