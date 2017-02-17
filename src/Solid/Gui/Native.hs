module Solid.Gui.Native where
#include <haskell>

solid :: IO ()
solid = do
  void initGUI
  window <- windowNew
  void $ on window deleteEvent $ do
    lift mainQuit
    return True
  set window [windowTitle := ("Solid" :: S.Text)]
  widgetShowAll window
  mainGUI
