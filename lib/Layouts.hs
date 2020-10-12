module Layouts
  ( myLayout
  )

where

import Themes (barTheme, tabTheme)

import           XMonad

import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Decoration
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing
import           XMonad.Layout.Simplest
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Named
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation


myLayout = tiled ||| mirrorTiled ||| full
  where
    tiled = named "[]="
      $ avoidStruts
      $ windowNavigation
      $ myGaps
      $ addTopBar
      $ addTabs shrinkText tabTheme
      $ mySpacing
      $ subLayout [] Simplest
      $ ResizableTall 1 (1/50) (2/3) []
    mirrorTiled = named "TTT"
      $ avoidStruts
      $ addTopBar
      $ myGaps
      $ mySpacing
      $ subLayout [] Simplest
      $ Mirror $ ResizableTall 1 (2/100) (1/2) []
    full = named "[*]"
      $ avoidStruts
      $ noBorders Full
    addTopBar = noFrillsDeco shrinkText barTheme
    myGaps = gaps [(U, 10), (D, 10), (L, 10), (R, 10)]
    mySpacing = spacing 10
