module Log 
  ( myLogHook
  , safePrintToPipe
  ) where

import Themes
import           System.IO
import           XMonad hiding ( logHook )
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.WorkspaceCompare

xmobarFont :: Int -> String -> String
xmobarFont f  = wrap (concat ["<fn-", show f, ">"]) "</fn>"

topBarPP :: PP
topBarPP = def
          { ppTitle   = xmobarColor "#C4C4C4" "" . shorten 50
          , ppCurrent = xmobarColor "#3579A8" "" . wrap "[" "]"
          , ppSep     = xmobarColor purple  "" " | "
          , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
          }

botBarPP :: PP
botBarPP = def

safePrintToPipe :: Maybe Handle -> String -> IO ()
safePrintToPipe = maybe (\_ -> return ()) hPutStrLn

myLogHook :: X ()
myLogHook = do
  t <- getNamedPipe "xmobarTop"
  dynamicLogWithPP $ topBarPP
    { ppOutput = safePrintToPipe t
    , ppTitle   = xmobarColor "#C4C4C4" "" . shorten 50
    , ppVisible = xmobarColor "#C4C4C4" "" . wrap "" ""
    , ppSep     = xmobarColor purple  "" " | "
    , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    }
