{-
                                                                  _/   
   _/    _/  _/_/_/  _/_/      _/_/    _/_/_/      _/_/_/    _/_/_/    
    _/_/    _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/     
 _/    _/  _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/      
_/    _/  _/    _/    _/    _/_/    _/    _/    _/_/_/    _/_/_/       

-}                                                                      

import Bindings
import Topics
import Themes
import Layouts
import Scratchpads
import Log

import           XMonad.Util.NamedScratchpad
import qualified Data.Map as M
import           Data.List (isPrefixOf)

import           XMonad

import           XMonad.Actions.TopicSpace
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.ManageHook
import           XMonad.StackSet as W
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.SpawnNamedPipe

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.SetWMName

import           System.IO

--------------------------------------------------------------------------------
-- main
-- xmproc <- spawnPipe "xmobar /home/kento/.xmonad/xmobar_top" "xmobarTop"

main = do
  checkTopicConfig myTopics myTopicConfig
  xmonad $ docks defaultConfig
    { layoutHook = myLayout
    , focusFollowsMouse = False
    , XMonad.workspaces = myTopics
    , logHook = myLogHook
    , borderWidth        = 0
    , normalBorderColor  = black
    , focusedBorderColor = purple
    , modMask            = mod4Mask
    , keys               = myKeys
    , startupHook        = myStartupHook
    , mouseBindings      = myMouseBindings
    , manageHook         = myManageHook
    }


myStartupHook :: X ()
myStartupHook = do
  spawnNamedPipe "xmobar ~/.xmonad/xmobar_top" "xmobarTop"
  spawnNamedPipe "xmobar ~/.xmonad/xmobar_bot" "xmobarBot"
  setWMName "xmonad"

--------------------------------------------------------------------------------
myManageHook = composeAll
  [ myNSManageHook
  , dialogHook
  , thunbirHook
  ]

myNSManageHook :: ManageHook
myNSManageHook = namedScratchpadManageHook pads

dialogHook = composeOne
  [ isDialog -?> doFloat ]

thunbirHook = composeAll
  [ className =? "Msgcompose" --> doFloat ]
