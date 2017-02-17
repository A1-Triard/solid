module Solid.Gui.Native where
#include <haskell>
import Paths_solid

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
    setSourceRGB 0.7 0.7 0.7
    rectangle 0.0 0.0 20.0 20.0
    fill
  widgetShowAll window
  mainGUI
