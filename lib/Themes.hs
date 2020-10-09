module Themes 
  ( barTheme
  , myXPConfig
  , black
  , purple)

where

import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Layout.NoFrillsDecoration

myFont = "xft:Dina:bold:size=10:antialias=true" 
black = "#282828"
purple = "#8f3f71"

barTheme =
    def
      { fontName = font
      , inactiveBorderColor = black
      , inactiveColor = black
      , inactiveTextColor = black
      , activeBorderColor = purple
      , activeColor = purple
      , activeTextColor = purple
      , urgentTextColor = purple
      , urgentBorderColor = purple
      , decoHeight = decorationHeight
      }
    where
      decorationHeight = 7
      font = "xft:monospace:size=10" -- doesn't matter because of `shrinkText`-

myXPConfig :: XPConfig
myXPConfig = greenXPConfig 
  { font = myFont
  , bgColor = "#282828"
  , fgColor = "#8f3f71"
  , bgHLight = "#665c54"
  , fgHLight = "#1d2021"
  , borderColor = "#3c3836"
  , promptBorderWidth = 2
  , position = Bottom
  , height = 25
  }
