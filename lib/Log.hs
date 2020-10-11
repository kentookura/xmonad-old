module Log 
  ( myLogHook
  , safePrintToPipe
  , myFadeHook
  ) where

import Themes
import           System.IO
import           Data.IORef
import qualified Data.Set as S
import           Control.Monad (filterM, liftM, join)
import           XMonad hiding ( logHook )
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.WorkspaceCompare
import           XMonad.Hooks.FadeInactive

xmobarFont :: Int -> String -> String
xmobarFont f  = wrap (concat ["<fn-", show f, ">"]) "</fn>"

myLogHook :: X ()
myLogHook = do
  t <- getNamedPipe "xmobarTop"
  b <- getNamedPipe "xmobarBot"
  dynamicLogWithPP $ botBarPP
                   { ppOutput  = safePrintToPipe t
                   , ppTitle   = xmobarColor white "" . shorten 50
                   , ppVisible = xmobarColor white "" . wrap "" ""
                   , ppSep     = xmobarColor purple  "" " | "
                   , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
                   }
  dynamicLogWithPP $ topBarPP
                   { ppOutput  = safePrintToPipe b
                   , ppTitle   = xmobarColor white "" . shorten 50
                   , ppVisible = xmobarColor white "" . wrap "" ""
                   , ppSep     = xmobarColor purple  "" " | "
                   , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
                   }

topBarPP :: PP
topBarPP = def
         { ppTitle   = xmobarColor white "" . shorten 50
         , ppCurrent = xmobarColor blue "" . wrap "[" "]"
         , ppSep     = xmobarColor purple  "" " | "
         , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
         }

botBarPP :: PP
botBarPP = def
         { ppTitle   = xmobarColor white "" . shorten 50
         , ppCurrent = xmobarColor blue "" . wrap "[" "]"
         , ppSep     = xmobarColor purple  "" " | "
         , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
         }

safePrintToPipe :: Maybe Handle -> String -> IO ()
safePrintToPipe = maybe (\_ -> return ()) hPutStrLn

myFadeHook toggleFadeSet = fadeOutLogHook $ fadeIf (testCondition toggleFadeSet) 1
fadeOutWindows = className =? "alacritty" 

testCondition :: IORef (S.Set Window) -> Query Bool
testCondition floats =
  liftM not fadeOutWindows <&&> isUnfocused
  <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s | w `S.member` s = S.delete w s
                  | otherwise = S.insert w s
