module Themes 
  ( barTheme
  , tabTheme
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
      font = myFont

tabTheme =
    def
      { fontName = font
      , inactiveBorderColor = black
      , inactiveColor = black
      , inactiveTextColor = black
      , activeBorderColor = purple
      , activeColor = purple
      , activeTextColor = black
      , urgentTextColor = purple
      , urgentBorderColor = purple
      }
    where
      font = myFont

myXPConfig :: XPConfig
myXPConfig = greenXPConfig 
  { font = myFont
  , bgColor = black
  , fgColor = purple
  , bgHLight = "#665c54"
  , fgHLight = "#1d2021"
  , borderColor = "#3c3836"
  , promptBorderWidth = 2
  , position = Bottom
  , height = 25
  }
