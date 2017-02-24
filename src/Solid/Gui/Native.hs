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

springDraw :: Vector RigidBody -> Spring -> Render ()
springDraw b spring = do
  let i1 = springBodyIndex1 spring
  let i2 = springBodyIndex2 spring
  let b1 = fromMaybe (error "") $ b !? i1
  let b2 = fromMaybe (error "") $ b !? i2
  let c1 = bodyPosition b1
  let c2 = bodyPosition b2
  let p1 = c1 + (L.rotate (bodyDirection b1) $ springBodyPoint1 spring)
  let p2 = c2 + (L.rotate (bodyDirection b2) $ springBodyPoint2 spring)
  setSourceRGB 0.8 0.8 0.8
  setLineWidth 1.0
  moveTo (p1 ^._x) (-(p1 ^._y))
  lineTo (p2 ^._x) (-(p2 ^._y))
  stroke
  setSourceRGB (bodyColor b1 ^._x) (bodyColor b1 ^._y) (bodyColor b1 ^._z)
  moveTo (c1 ^._x) (-(c1 ^._y))
  lineTo (p1 ^._x) (-(p1 ^._y))
  stroke
  setSourceRGB (bodyColor b2 ^._x) (bodyColor b2 ^._y) (bodyColor b2 ^._z)
  moveTo (c2 ^._x) (-(c2 ^._y))
  lineTo (p2 ^._x) (-(p2 ^._y))
  stroke

systemDraw :: System -> Render ()
systemDraw s = do
  let ke = kineticEnergy $ bodies s
  let pe = potentialEnergy $ springs s
  let fe = ke + pe
  forM_ (bodies s) bodyDraw
  forM_ (springs s) $ springDraw (bodies s)
  selectFontFace ("monospace" :: S.Text) FontSlantNormal FontWeightNormal
  setFontSize 14
  setSourceRGB 0.9 0.9 0.9
  moveTo 10.0 20.0
  showText $ "Time = " ++ show (round $ time s :: Int)
  moveTo 10.0 40.0
  showText $ "Kinetic Energy = " ++ show (round ke :: Int)
  moveTo 10.0 60.0
  showText $ "Potential Energy = " ++ show (round pe :: Int)
  moveTo 10.0 80.0
  showText $ "Energy = " ++ show (round fe :: Int)

testBodies :: Vector RigidBody
testBodies = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (50.0 *^ (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)))
      (V3 300.0 (-300.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 0.0)
  , RigidBody
      (V3 0.5 0.6 0.7)
      1.0
      (1e3 *^ (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)))
      (V3 600.0 (-600.0) 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) pi)
      (V3 0.0 0.0 0.0)
  ]

nan :: Double
nan = 0.0 / 0.0

testSprings :: Vector Spring
testSprings = V.fromList
  [ Spring
      0.2
      25.0
      0 (V3 50.0 (-50.0) 0.0)
      1 (V3 50.0 50.0 0.0)
      (V3 nan nan nan)
      (V3 nan nan nan)
      nan
      (V3 nan nan nan)
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
