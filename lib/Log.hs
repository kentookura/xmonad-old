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
import qualified XMonad.StackSet as W
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
  l <- getNamedPipe "xmobarTopLeft"
  t <- getNamedPipe "xmobarTop"
  r <- getNamedPipe "xmobarTopRight"
  b <- getNamedPipe "xmobarBot"
  dynamicLogWithPP $ topBarPP { ppOutput  = safePrintToPipe l}
  dynamicLogWithPP $ topBarPP { ppOutput  = safePrintToPipe t}
  dynamicLogWithPP $ topBarPP { ppOutput  = safePrintToPipe r}
  dynamicLogWithPP $ botBarPP { ppOutput  = safePrintToPipe b }

windowCount :: X (Maybe String)
windowCount = gets $ Just . wrap "{ " " }" . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

topBarPP :: PP
topBarPP = def
         { ppTitle   = xmobarColor white "" . shorten 150
         , ppCurrent = xmobarColor blue "" . wrap "(" ")"
         , ppVisible = xmobarColor white "" . wrap "" " *"
         , ppSep     = xmobarColor purple  "" " | "
         , ppExtras  = [windowCount] 
         , ppSort    = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
         }

botBarPP :: PP
botBarPP = def
  { ppCurrent = const ""
  , ppVisible = const ""
  , ppUrgent  = const ""
  , ppLayout  = const ""
  , ppTitle   = const ""
  , ppSep     = ""
  , ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
  }

todoLogger = undefined

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
