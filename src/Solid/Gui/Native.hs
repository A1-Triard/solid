module Solid.Gui.Native where

#include <haskell>
import Solid.Solver
import Paths_solid

drawBody :: V2 Double -> RigidBody -> Render ()
drawBody c b = do
  let v = L.rotate (bodyDirection b) (V3 20.0 0.0 0.0)
  let v1 = bodyPosition b + v
  let v2 = bodyPosition b + V3 (v ^._y) (-(v ^._x)) (v ^._z)
  let v3 = bodyPosition b + V3 (-(v ^._x)) (-(v ^._y)) (v ^._z)
  let v4 = bodyPosition b + V3 (-(v ^._y)) (v ^._x) (v ^._z)
  setSourceRGB (bodyColor b ^._x) (bodyColor b ^._y) (bodyColor b ^._z)
  setLineWidth 1.0
  moveTo (c ^._x + v1 ^._x) (c ^._y - v1 ^._y)
  lineTo (c ^._x + v3 ^._x) (c ^._y - v3 ^._y)
  lineTo (c ^._x + v4 ^._x) (c ^._y - v4 ^._y)
  lineTo (c ^._x + v2 ^._x) (c ^._y - v2 ^._y)
  lineTo (c ^._x + v1 ^._x) (c ^._y - v1 ^._y)
  fill

drawSpring :: V2 Double -> Vector RigidBody -> Spring -> Render ()
drawSpring c b spring = do
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
  moveTo (c ^._x + p1 ^._x) (c ^._y - p1 ^._y)
  lineTo (c ^._x + p2 ^._x) (c ^._y - p2 ^._y)
  stroke
  setSourceRGB (bodyColor b1 ^._x) (bodyColor b1 ^._y) (bodyColor b1 ^._z)
  moveTo (c ^._x + c1 ^._x) (c ^._y - c1 ^._y)
  lineTo (c ^._x + p1 ^._x) (c ^._y - p1 ^._y)
  stroke
  setSourceRGB (bodyColor b2 ^._x) (bodyColor b2 ^._y) (bodyColor b2 ^._z)
  moveTo (c ^._x + c2 ^._x) (c ^._y - c2 ^._y)
  lineTo (c ^._x + p2 ^._x) (c ^._y - p2 ^._y)
  stroke

drawSystem :: V2 Double -> System -> Render ()
drawSystem c s = do
  forM_ (bodies s) $ drawBody c
  forM_ (springs s) $ drawSpring c (bodies s)
  selectFontFace ("monospace" :: S.Text) FontSlantNormal FontWeightNormal
  setFontSize 14
  setSourceRGB 0.9 0.9 0.9
  moveTo 10.0 20.0
  showText $ "Time = " ++ show (round $ time s :: Int)
  moveTo 10.0 40.0
  showText $ "Energy = " ++ show (round (kineticEnergy s + potentialEnergy s) :: Int)

testBodies :: Vector RigidBody
testBodies = V.fromList
  [ RigidBody
      (V3 0.7 0.6 0.5)
      1.0
      (50.0 *^ (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)))
      (V3 (-150.0) 150.0 0.0)
      (V3 0.0 0.0 0.0)
      (L.axisAngle (V3 0.0 0.0 1.0) (0.5 * pi))
      (V3 0.0 0.0 0.0)
  , RigidBody
      (V3 0.5 0.6 0.7)
      1.0
      (1e3 *^ (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)))
      (V3 150.0 (-150.0) 0.0)
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
  { timer :: !(IO Double)
  , canvas :: !DrawingArea
  , normalizeQ :: !CheckMenuItem
  , keDiagram :: !Diagram
  , peDiagram :: !Diagram
  }

queueFrame :: UI -> System -> IO ()
queueFrame ui s = do
  Rectangle _ _ w h <- widgetGetAllocation $ canvas ui
  let y0 = fromIntegral h - 30.0
  draw_id <- on (canvas ui) draw $ do
    drawSystem (V2 (fromIntegral w / 2.0) (50.0 + (fromIntegral h - 300.0) / 2.0)) s
    setLineWidth 1.0
    setSourceRGB 0.8 0.8 0.8
    moveTo 20.0 y0
    lineTo 1040.0 y0
    stroke
    moveTo 30.0 (y0 + 10.0)
    lineTo 30.0 (y0 - 200.0)
    stroke
    drawDiagram (V2 25.0 y0) (V3 0.3 0.6 0.6) (keDiagram ui)
    drawDiagram (V2 25.0 y0) (V3 0.6 0.6 0.3) (peDiagram ui)
  void $ timeoutAdd (frame ui s draw_id) $ round (1000.0 / fps)

frame :: UI -> System -> ConnectId DrawingArea -> IO Bool
frame ui s draw_id = do
  t <- timer ui
  nq <- checkMenuItemGetActive $ normalizeQ ui
  signalDisconnect draw_id
  let s1 = advance t nq s
  let i = floor (20.0 * t)
  let ui1 = ui { keDiagram = addPoint i (kineticEnergy s / 100.0) (keDiagram ui), peDiagram = addPoint i (potentialEnergy s / 100.0) (peDiagram ui) }
  queueFrame ui1 s1
  widgetQueueDraw $ canvas ui1
  return False

data Diagram = Diagram !Int !(Vector (Maybe Double))

addPoint :: Int -> Double -> Diagram -> Diagram
addPoint m v (Diagram ni d) =
  go (m `mod` V.length d)
  where
    go i
      | i == ni = Diagram ni d
      | i > ni = Diagram i $ d // [(x, Just v) | x <- [(ni + 1) .. i]]
      | otherwise = Diagram i $ d // [(x, Just v) | x <- [(ni + 1) .. (V.length d - 1)]] // [(x, Just v) | x <- [0 .. i]]

drawDiagram :: V2 Double -> V3 Double -> Diagram -> Render ()
drawDiagram (V2 x0 y0) (V3 cr cg cb) (Diagram ni d) = do
  setLineWidth 1.0
  setSourceRGB cr cg cb
  evalStateT go (x0, 0, True)
  stroke
  where
    go :: StateT (Double, Int, Bool) Render ()
    go = V.forM_ d $ \my -> do
      (x, i, s) <- get
      case my of
        Nothing -> put (x + 2.0, i + 1, s)
        Just y -> do
          if s
            then lift $ moveTo x (y0 - y)
            else return ()
          if i == ni + 1
            then do
              lift stroke
              put (x + 2.0, i + 1, True)
            else do
              lift $ lineTo x (y0 - y)
              put (x + 2.0, i + 1, False)

fps :: Double
fps = 50.0

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
      , keDiagram = Diagram 0 (V.replicate 500 Nothing)
      , peDiagram = Diagram 0 (V.replicate 500 Nothing)
      }
  queueFrame ui $ start dt testSprings testBodies
  widgetShowAll window
  mainGUI
