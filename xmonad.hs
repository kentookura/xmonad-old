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

import           XMonad.Util.NamedScratchpad
import qualified Data.Map as M
import           Data.List (isPrefixOf)

import           XMonad

import           XMonad.Actions.TopicSpace

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.ManageHook
import           XMonad.StackSet as W

import           XMonad.Util.Run(spawnPipe)

import           System.IO

--------------------------------------------------------------------------------
-- main

main = do
  xmproc <- spawnPipe "xmobar /home/kento/.xmonad/xmobar"
  checkTopicConfig myTopics myTopicConfig
  xmonad $ docks defaultConfig
    { layoutHook = myLayout
    , focusFollowsMouse = False
    , XMonad.workspaces = myTopics
    , logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "#C4C4C4" "" . shorten 50
              , ppCurrent = xmobarColor "#3579A8" "" . wrap "[" "]"
              }
    , borderWidth = 0
    , normalBorderColor = black
    , focusedBorderColor = purple
    , modMask = mod4Mask
    , keys = myKeys
    , mouseBindings = myMouseBindings
    , manageHook = manageHooks
    }

--------------------------------------------------------------------------------
manageHooks = namedScratchpadManageHook pads <+> composeOne
  [ ("uni" `isPrefixOf`) <$> title -?> doShift "uni" ]
