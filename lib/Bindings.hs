module Bindings
  ( myKeys
  , myMouseBindings
  )
where 
--{{{
import Themes
import Topics
import Scratchpads
import Tree

import qualified Data.Map as M
import System.IO
import System.Exit
import XMonad

import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.TopicSpace
import qualified XMonad.Actions.TreeSelect as TS
import           XMonad.Hooks.ManageDocks
import           XMonad.Prompt.Workspace
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Operations
import           XMonad.Prompt.Workspace
import           XMonad.StackSet
import           XMonad.StackSet as W
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.NamedScratchpad
--}}}

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ 
  -- scratchpads and prompts
  [ ((modMask                , xK_space), namedScratchpadAction pads "term")
  , ((modMask .|. mod1Mask   , xK_t),     namedScratchpadAction pads "htop")
  , ((modMask .|. mod1Mask   , xK_f),     namedScratchpadAction pads "pfetch")
  , ((modMask .|. mod1Mask   , xK_s),     namedScratchpadAction pads "mail")
  , ((modMask .|. mod1Mask   , xK_d),     namedScratchpadAction pads "discord")
  , ((modMask .|. mod1Mask   , xK_c),     namedScratchpadAction pads "cava")
  , ((modMask .|. mod1Mask   , xK_w),     namedScratchpadAction pads "wiki")

  -- programs
  , ((modMask                 , xK_Return), spawnShell )
  , ((modMask .|. shiftMask   , xK_Return), spawn "alacritty")
  , ((modMask                 , xK_d),      spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi run, drun -theme gruvbox-dark-hard")
  , ((modMask                 , xK_w),      spawn "qutebrowser")
  , ((modMask                 , xK_r),      spawn "alacritty -e ranger")
  , ((controlMask .|. mod1Mask, xK_l),      spawn "lock")

  -- layout
  , ((modMask              , xK_Tab),    sendMessage NextLayout)
  , ((modMask              , xK_f),      sendMessage $ JumpToLayout "[ ]")
  , ((modMask              , xK_t),      treeselectAction treeTheme)
  , ((modMask              , xK_b),      sendMessage ToggleStruts)
  , ((modMask .|. shiftMask, xK_h),      sendMessage (IncMasterN( 1)))
  , ((modMask .|. shiftMask, xK_l),      sendMessage (IncMasterN(-1)))

  -- focus
  , ((modMask .|. shiftMask, xK_space),  windows W.swapMaster)
  , ((modMask              , xK_j),      windows W.focusDown)
  , ((modMask              , xK_k),      windows W.focusUp)
  , ((modMask .|. shiftMask, xK_j),      windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k),      windows W.swapUp)


  , ((modMask .|. mod1Mask, xK_h), sendMessage $ pullGroup L)
  , ((modMask .|. mod1Mask, xK_l), sendMessage $ pullGroup R)
  , ((modMask .|. mod1Mask, xK_k), sendMessage $ pullGroup U)
  , ((modMask .|. mod1Mask, xK_j), sendMessage $ pullGroup D)

  , ((modMask .|. mod1Mask, xK_m), withFocused (sendMessage . MergeAll))
  , ((modMask .|. mod1Mask, xK_u), withFocused (sendMessage . UnMerge))

  , ((modMask .|. mod1Mask, xK_g), onGroup W.focusUp')
  , ((modMask .|. mod1Mask, xK_l), onGroup W.focusDown')

  -- resizing
  , ((modMask              , xK_h), sendMessage Shrink)
  , ((modMask              , xK_l), sendMessage Expand)
  , ((modMask              , xK_u), sendMessage MirrorShrink)
  , ((modMask              , xK_i), sendMessage MirrorExpand)

  -- floating
  , ((modMask .|. shiftMask, xK_t    ), withFocused $ windows . W.sink)

  , ((modMask              , xK_Up   ), withFocused (keysMoveWindow (0  , -10)))
  , ((modMask              , xK_Down ), withFocused (keysMoveWindow (0  , 10)))
  , ((modMask              , xK_Right), withFocused (keysMoveWindow (10 ,  0)))
  , ((modMask              , xK_Left ), withFocused (keysMoveWindow (-10,  0)))

  , ((modMask .|. shiftMask, xK_Up   ), withFocused (keysResizeWindow (0  , 10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Down ), withFocused (keysResizeWindow (0  ,-10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Right), withFocused (keysResizeWindow (10 ,  0) (0, 0)))
  , ((modMask .|. shiftMask, xK_Left ), withFocused (keysResizeWindow (-10,  0) (0, 0)))

  -- util
  , ((modMask .|. shiftMask, xK_c),      kill)  
  , ((modMask              , xK_q),      restart "xmonad" True)
  , ((modMask .|. shiftMask, xK_q),      io (exitWith ExitSuccess))

  -- topics
  , ((modMask              , xK_a),      currentTopicAction myTopicConfig)
  , ((modMask              , xK_g),      promptedGoto)
  , ((modMask .|. shiftMask, xK_g),      promptedShift)

  , ((shiftMask, xK_F2), spawn "pamixer -d 5")
  , ((shiftMask, xK_F2), spawn "pamixer -i 5")]
  ++
  -- screens
  [((m .|. modMask, key), f sc)
    | (key, sc) <- zip [xK_m, xK_comma, xK_period] [0..]
    , (f, m)    <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask,               button1), \w -> XMonad.Operations.focus w >> mouseMoveWindow w )
  , ((modMask .|. shiftMask, button1), \w -> XMonad.Operations.focus w >> Flex.mouseResizeWindow w )
  ]

