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
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Set as S

import           XMonad

import           XMonad.Actions.TopicSpace
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.EwmhDesktops
import           XMonad.ManageHook
import           XMonad.StackSet as W
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.SpawnNamedPipe

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.SetWMName

import           System.IO

--------------------------------------------------------------------------------

main = do
  checkTopicConfig myTopics myTopicConfig
  toggleFadeSet <- newIORef S.empty
  xmonad 
    $ ewmh
    $ docks defaultConfig
    { layoutHook         = myLayout
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , XMonad.workspaces  = myTopics
    , logHook            = myLogHook <> myFadeHook toggleFadeSet
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
  spawnNamedPipe "xmobar ~/.xmonad/xmobar/xmobar_top" "xmobarTop"
  spawnNamedPipe "xmobar ~/.xmonad/xmobar/xmobar_bot" "xmobarBot"

--------------------------------------------------------------------------------
 
myManageHook :: ManageHook
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
