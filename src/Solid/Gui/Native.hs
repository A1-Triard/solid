module Solid.Gui.Native where

#include <haskell>
import Data.MatrixSpace
import Paths_solid

data RigidBody = RigidBody
  { bodyColor :: Vec 3 Double
  , bodyMass :: Double
  , bodyPosition :: Vec 3 Double
  , bodyVelocity :: Vec 3 Double
  , bodyDirection :: (Double, Vec 3 Double)
  , bodyAngularVelocity :: Vec 3 Double
  }

data System = System
  { time :: !Double
  , bodies :: [RigidBody]
  }

bodyDraw :: RigidBody -> Render ()
bodyDraw b = do
  let v = qrotate (bodyDirection b) (vec3 20.0 0.0 0.0)
  let v1 = bodyPosition b ^+^ v
  let v2 = bodyPosition b ^+^ vec3 (vecY v) (-(vecX v)) (vecZ v)
  let v3 = bodyPosition b ^+^ vec3 (-(vecX v)) (-(vecY v)) (vecZ v)
  let v4 = bodyPosition b ^+^ vec3 (-(vecY v)) (vecX v) (vecZ v)
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
      (vec3 0.7 0.6 0.5)
      1.0
      (vec3 300.0 300.0 0.0)
      (vec3 100.0 0.0 0.0)
      (qrotation (0.3 * pi) (vec3 0.0 0.0 1.0))
      (vec3 0.0 0.0 1.0)
  ]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

advancePosition :: Double -> [RigidBody] -> [RigidBody]
advancePosition d s =
  map (\x -> x
    { bodyPosition = bodyPosition x ^+^ (d *^ bodyVelocity x)
    , bodyDirection = bodyDirection x -- ^+^ (d *^ (bodyAngularVelocity x `cross3` bodyDirection x))
    }) s

{-
advanceVelosity :: Double -> [RigidBody] -> [RigidBody]
advanceVelosity d s =
  map (\x -> let a = 
    x
    { bodyVelocity = bodyVelocity x ^+^ Vec3 (100.0 * d) 0.0 0.0
    --, bodyAngularVelocity = bodyDirection x ^+^ (d *^ (bodyAngularVelocity x `crossProduct` bodyDirection x))
    }) s
-}

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
