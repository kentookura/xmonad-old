import qualified Data.Map as M
import           System.Exit
import           XMonad
import           XMonad.StackSet
import           XMonad.Operations
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.TopicSpace
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Named
import           XMonad.Layout.Spacing
import           XMonad.ManageHook
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.StackSet as W
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run(spawnPipe)

import           System.IO

-- main
--------------------------------------------------------------------------------

main = do
  xmproc <- spawnPipe "xmobar /home/kento/.xmonad/xmobar"
  checkTopicConfig myTopics myTopicConfig
  xmonad $ docks defaultConfig
    { layoutHook = spacingRaw True (Border 0 0 0 0) True (Border 10 10 5 5) True myLayout
    , XMonad.workspaces = myTopics
    , logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "#C4C4C4" "" . shorten 50
              , ppCurrent = xmobarColor "#3579A8" "" . wrap "[" "]"
              }
    , borderWidth = 2
    , normalBorderColor = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , modMask = modMask'
    , keys = keys'
    , mouseBindings = myMouseBindings
    , manageHook = namedScratchpadManageHook scratchpads
    }

--------------------------------------------------------------------------------
-- looks

normalBorderColor' = "#282828"
focusedBorderColor' = "#8f3f71"

--------------------------------------------------------------------------------
-- layouts

myLayout = avoidStruts $ named "[]=" (smartBorders tiled) ||| named "TTT" (smartBorders (Mirror tiled)) ||| named "[M]" (noBorders Full)
  where
    tiled = ResizableTall 1 (2/100) (1/2) []

--------------------------------------------------------------------------------
-- xprompt

myXPConfig :: XPConfig
myXPConfig = greenXPConfig 
  { font = "xft:Dina:bold:size=10:antialias=true" 
  , bgColor = "#282828"
  , fgColor = "#8f3f71"
  , bgHLight = "#665c54"
  , fgHLight = "#1d2021"
  , borderColor = "#3c3836"
  , promptBorderWidth = 2
  , position = Bottom
  , height = 25
  }

--------------------------------------------------------------------------------
-- topics
-- todo: email topic

myTopics :: [Topic]
myTopics = ["none"
           , "xm"
           , "uni"
           , "config"
           , "site"
           , "cv"
           , "docs"
           , "hsk"
           , "web"]

myTopicConfig :: TopicConfig
myTopicConfig = def
  -- associate directory with topic
  { topicDirs = M.fromList
    [ ("none", "")
    , ("config", ".config")
    , ("uni", "uni")
    , ("site", "site")
    , ("docs", "doc")
    , ("xm", ".xmonad")
    , ("cv", "doc/cv")
    , ("web", "dl")
    ]
  , topicActions = M.fromList
    [ ("config", spawn "alacritty"
              >> spawn "alacritty")
    , ("docs",   spawn "zathura ~/doc/haskell.pdf")
    , ("cv",     spawn "zathura doc/cv/output/resume.pdf" 
              >> spawn "alacritty --working-directory doc/cv/markdown/"
              >> spawn "alacritty -e vim doc/cv/markdown/resume.md")
    , ("uni", spawn "alacritty -e ranger uni" >> spawnShell)
    , ("site",   spawn "alacritty --working-directory site/src"
              >> spawn "alacritty --working-directory site/src/templates"
              >> spawn "alacritty --working-directory site -e stack exec site watch"
              >> spawn "qutebrowser http://localhost:8000")
    , ("xm",     spawn "alacritty -e vim .xmonad/xmonad.hs"
              >> spawnShell)
    ]
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "alacritty --working-directory " ++ dir

spawnCmdIn :: Dir -> String -> X ()
spawnCmdIn dir cmd = spawn $ "alacritty --working-directory " ++ dir ++ " -e " ++ cmd

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShfit :: X ()
promptedShfit = workspacePrompt myXPConfig $ windows . W.shift

--------------------------------------------------------------------------------
-- scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "htop" "alacritty -t htop -e /bin/htop" (title =? "htop")
                  ( customFloating $ W.RationalRect (1/3) (1/37) (2/3) (2/3)) 
              , NS "pfetch" "alacritty --hold -t pfetch -e /bin/pfetch" (title =? "pfetch")
                  ( customFloating $ W.RationalRect (1/6) (1/37) (1/3) (1/3))
              , NS "cava" "alacritty --hold -t cava -e /bin/cava" (title =? "cava")
                  ( customFloating $ W.RationalRect (4/6) (1/37) (1/3) (1/3))
              , NS "watch" "alacritty --working-directory site/ --hold -t watch -e stack exec site watch" (title =? "watch")
                  ( customFloating $ W.RationalRect (0) (1/37) (1/3) (2/3))
              , NS "term" "alacritty -t term" (title =? "term")
                  ( customFloating $ W.RationalRect (1/3) (1/37) (2/3) (2/3))
              ]
--------------------------------------------------------------------------------
-- keys

modMask' = mod4Mask
workspaceKeys = [xK_1..xK_9] ++ [xK_0]

keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ 
  [
  -- scratchpads and prompts
    ((modMask .|. mod1Mask   , xK_t), namedScratchpadAction scratchpads "htop")
  , ((modMask .|. mod1Mask   , xK_space), namedScratchpadAction scratchpads "term")
  , ((modMask .|. mod1Mask   , xK_f), namedScratchpadAction scratchpads "pfetch")
  , ((modMask .|. mod1Mask   , xK_c), namedScratchpadAction scratchpads "cava")
  , ((modMask .|. mod1Mask   , xK_w), namedScratchpadAction scratchpads "watch")

  -- programs
  , ((modMask              , xK_Return), spawn "alacritty" )
  , ((modMask              , xK_d),      spawn "dmenu_run")
  , ((modMask              , xK_w),      spawn "qutebrowser")
  , ((modMask              , xK_r),      spawn "alacritty -e ranger")

  -- layout
  , ((modMask              , xK_Tab),    sendMessage NextLayout)
  , ((modMask              , xK_t),      setLayout $ XMonad.layoutHook conf)
  , ((modMask              , xK_b),      sendMessage ToggleStruts)
  , ((modMask .|. shiftMask, xK_h),      sendMessage (IncMasterN( 1)))
  , ((modMask .|. shiftMask, xK_l),      sendMessage (IncMasterN(-1)))

  -- focus
  , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((modMask              , xK_j),      windows W.focusDown)
  , ((modMask              , xK_k),      windows W.focusUp)
  , ((modMask .|. shiftMask, xK_j),      windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k),      windows W.swapUp)

  -- resizing
  , ((modMask              , xK_h),      sendMessage Shrink)
  , ((modMask              , xK_l),      sendMessage Expand)
  , ((modMask              , xK_u),      sendMessage MirrorShrink)
  , ((modMask              , xK_i),      sendMessage MirrorExpand)

  -- floating
  , ((modMask              , xK_space),  withFocused $ windows . W.sink)

  , ((modMask              , xK_Up   ),  withFocused (keysMoveWindow (0  , -10)))
  , ((modMask              , xK_Down ),  withFocused (keysMoveWindow (0  , 10)))
  , ((modMask              , xK_Right),  withFocused (keysMoveWindow (10 ,  0)))
  , ((modMask              , xK_Left ),  withFocused (keysMoveWindow (-10,  0)))

  , ((modMask .|. shiftMask, xK_Up   ),  withFocused (keysResizeWindow (0  , 10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Down ),  withFocused (keysResizeWindow (0  ,-10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Right),  withFocused (keysResizeWindow (10 ,  0) (0, 0)))
  , ((modMask .|. shiftMask, xK_Left ),  withFocused (keysResizeWindow (-10,  0) (0, 0)))

  -- util
  , ((modMask .|. shiftMask, xK_c),      kill)  
  , ((modMask              , xK_q),      restart "xmonad" True)
  , ((modMask .|. shiftMask, xK_q),      io (exitWith ExitSuccess))

  -- topics
  , ((modMask              , xK_a),      currentTopicAction myTopicConfig)
  , ((modMask              , xK_g),      promptedGoto)
  , ((modMask .|. shiftMask, xK_g),      promptedShfit)]
  ++
  [ ((modMask              , k),         switchNthLastFocused myTopicConfig i) 
    | (i, k) <- zip [1..] workspaceKeys] 
  ++ 
  [ ((modMask .|. shiftMask, k),         shiftNthLastFocused i) 
    | (i, k) <- zip [1..] workspaceKeys]
  ++ 
  -- screens
  [((m .|. modMask, key), f sc)
    | (key, sc) <- zip [xK_m, xK_comma, xK_period] [0..]
    , (f, m)    <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask,               button1), \w -> XMonad.Operations.focus w >> mouseMoveWindow w )
  , ((modMask .|. shiftMask, button1), \w -> XMonad.Operations.focus w >> Flex.mouseResizeWindow w )
  ]
