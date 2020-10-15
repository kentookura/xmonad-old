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
import           XMonad.Layout.NoBorders
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
        { startupHook        = myStartupHook
        , layoutHook         = myLayout
        , modMask            = mod4Mask
        , manageHook         = myManageHook
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , XMonad.workspaces  = myTopics
        , logHook            = myLogHook <> myFadeHook toggleFadeSet
        , borderWidth        = 0
        , normalBorderColor  = black
        , focusedBorderColor = purple
        }

--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  spawnNamedPipe "xmobar ~/.xmonad/xmobar/xmobar_top" "xmobarTop"
  spawnNamedPipe "xmobar ~/.xmonad/xmobar/xmobar_bot" "xmobarBot"
  spawnNamedPipe "xmobar -x 1 ~/.xmonad/xmobar/xmobar_top" "xmobarTopLeft"
  spawnNamedPipe "xmobar -x 2 ~/.xmonad/xmobar/xmobar_top" "xmobarTopRight"
 
myNSManageHook :: ManageHook
myNSManageHook = namedScratchpadManageHook pads

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [ myNSManageHook ]
  , [ title =? "x9term" --> doFloat ]
  , [ className =? "Msgcompose" --> doFloat ]
  ]
