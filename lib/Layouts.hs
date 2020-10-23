module Layouts
  ( myLayout
  )

where

import Themes (barTheme, tabTheme)

import qualified Data.Map as M

import           XMonad
import qualified XMonad.StackSet as W

import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.Decoration
import           XMonad.Layout.Gaps
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation

myLayout = tiled ||| mirrorTiled ||| full
  where
    tiled = named "[]="
      $ avoidStruts
      $ windowNavigation
--    $ myGaps
      $ addTopBar
--    $ mySpacing
      $ subLayout [] Simplest
      $ ResizableTall 1 (1/50) (2/3) []
    mirrorTiled = named "TTT"
      $ avoidStruts
--    $ myGaps
      $ addTopBar
      $ addTabs shrinkText tabTheme
--    $ mySpacing
      $ subLayout [] Simplest
      $ Mirror $ ResizableTall 1 (2/100) (1/2) []
    full = named "[*]"
      $ avoidStruts
      $ noBorders Full
    addTopBar = noFrillsDeco shrinkText barTheme
    myGaps = gaps [(U, 10), (D, 10), (L, 10), (R, 10)]
    mySpacing = spacing 10

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
  hiddens _ wset _ _ _ = M.keys $ W.floating wset

data AllTiled = AllTiled deriving (Read, Show)

instance SetsAmbiguous AllTiled where
  hiddens _ wset _ _ _ = M.keys $ W.floating wset
