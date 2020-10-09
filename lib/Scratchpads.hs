module Scratchpads
  (pads)
where

import           XMonad.StackSet as W
import           XMonad.Util.NamedScratchpad
import           XMonad.ManageHook

pads :: [NamedScratchpad]
pads = [ NS "htop" "alacritty -t htop -e /bin/htop" (title =? "htop") htopHook
       , NS "pfetch" "alacritty --hold -t pfetch -e /bin/pfetch" (title =? "pfetch") pfetchHook
       , NS "cava" "alacritty --hold -t cava -e /bin/cava" (title =? "cava") cavaHook
       , NS "watch" "alacritty --working-directory site/ --hold -t watch -e stack exec site watch" (title =? "watch") watchHook
       , NS "term" "alacritty -t term" (title =? "term") termHook
       ]
         where htopHook = ( customFloating $ rr (1/3) (1/37) (2/3) (2/3)) 
               pfetchHook = ( customFloating $ rr (1/6) (1/37) (1/3) (1/3))
               cavaHook = ( customFloating $ rr (4/6) (1/37) (1/3) (1/3))
               watchHook = ( customFloating $ rr (0) (1/37) (1/3) (2/3))
               termHook = ( customFloating $ rr (1/3) (1/37) (2/3) (2/3))
               rr = W.RationalRect
