module Scratchpads
  ( pads
  , padKeys)
where

import           XMonad
import           XMonad.Operations
import           XMonad.StackSet as W
import           XMonad.Util.NamedScratchpad
import           XMonad.ManageHook

pads :: [NamedScratchpad]
pads = [ NS "htop"    "alacritty -t htop -e /bin/htop" (title =? "htop") htopHook
       , NS "spotify" "alacritty -t spot -e spt"       (title =? "spot") spotHook
       , NS "torque"  "alacritty -t torque -e torque"  (title =? "torque") torqueHook
       , NS "nmtui"  "alacritty -t nmtui -e nmtui"  (title =? "nmtui") nmtuiHook
       , NS "discord" "discord"                        (className =? "discord") discordHook
       , NS "mail"    "thunderbird"                    (className =? "Thunderbird") discordHook
       , NS "wiki"    "alacritty -t wiki -e vim ~/wiki/index.md" (title =? "wiki") wikiHook
       , NS "pfetch"  "alacritty --hold -t pfetch -e /bin/pfetch" (title =? "pfetch") pfetchHook
       , NS "cava"    "alacritty --hold -t cava -e /bin/cava" (title =? "cava") cavaHook
       , NS "watch"   "alacritty --working-directory site/ --hold -t watch -e stack exec site watch" (title =? "watch") watchHook
       , NS "term"    "alacritty -t term"              (title =? "term") termHook
       ]
         where htopHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               spotHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               torqueHook  = customFloating $ rr (1/6) (1/37) (2/3) (2/3)
               nmtuiHook   = customFloating $ rr (1/6) (1/37) (2/3) (2/3)
               discordHook = customFloating $ rr   0   (1/37) (2/3) (2/3)
               thunbirHook = customFloating $ rr   0   (1/37) (2/3) (2/3)
               pfetchHook  = customFloating $ rr (1/8) (1/37) (1/3) (1/3)
               cavaHook    = customFloating $ rr (4/6) (1/37) (1/3) (1/3)
               watchHook   = customFloating $ rr   0   (1/37) (1/3) (2/3)
               termHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               wikiHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               rr = W.RationalRect
padKeys = 
  [ ((mod4Mask                , xK_space), namedScratchpadAction pads "term")
  , ((mod4Mask .|. mod1Mask   , xK_t),     namedScratchpadAction pads "torque")
  , ((mod4Mask .|. mod1Mask   , xK_q),     namedScratchpadAction pads "htop")
  , ((mod4Mask .|. mod1Mask   , xK_f),     namedScratchpadAction pads "pfetch")
  , ((mod4Mask .|. mod1Mask   , xK_n),     namedScratchpadAction pads "nmtui")
  , ((mod4Mask .|. mod1Mask   , xK_s),     namedScratchpadAction pads "spotify")
  , ((mod4Mask .|. mod1Mask   , xK_a),     namedScratchpadAction pads "mail")
  , ((mod4Mask .|. mod1Mask   , xK_d),     namedScratchpadAction pads "discord")
  , ((mod4Mask .|. mod1Mask   , xK_c),     namedScratchpadAction pads "cava")
  , ((mod4Mask .|. mod1Mask   , xK_w),     namedScratchpadAction pads "wiki")
  ]
