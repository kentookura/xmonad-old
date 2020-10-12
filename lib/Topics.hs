module Topics
  ( myTopics
  , myTopicConfig
  , spawnShell
  , spawnShellIn 
  , spawnCmdIn 
  , goto
  , promptedGoto 
  , promptedShift
  )

where

import           XMonad
import           Themes
import qualified Data.Map as M
import           XMonad.Actions.TopicSpace
import           XMonad.StackSet as W
import           XMonad.Prompt.Workspace

myTopics :: [Topic]
myTopics = [ "none"
           , "config"
           , "cv"
           , "docs"
           , "hsk"
           , "site"
           , "uni"
           , "wiki"
           , "xm"
           , "web"]

myTopicConfig :: TopicConfig
myTopicConfig = def
  -- associate directory with topic
  { topicDirs = M.fromList
    [ ("none", "./")
    , ("config", ".config")
    , ("cv", "doc/cv")
    , ("docs", "doc")
    , ("hsk", "hsk")
    , ("site", "site")
    , ("uni", "uni")
    , ("web", "dl")
    , ("wiki", "wiki")
    , ("xm", ".xmonad")
    ]
  , topicActions = M.fromList
    [
      ("config", spawn "alacritty"
              >> spawn "alacritty")
    , ("cv",     spawn "zathura doc/cv/output/resume.pdf" 
              >> spawn "alacritty --working-directory doc/cv/markdown/"
              >> spawn "alacritty -e vim doc/cv/markdown/resume.md")
    , ("docs",   spawn "zathura ~/doc/haskell.pdf")
    , ("site",   spawn "alacritty --working-directory site/src"
              >> spawn "alacritty --working-directory site/src/templates"
              >> spawn "qutebrowser http://localhost:8000")
    , ("uni",    spawn "alacritty --working-directory uni -e abduco -A uni-session dvtm"
              >> spawn "zathura ~/uni/log/script.pdf"
              >> spawn "zathura ~/uni/alg/Algebra_2_13.pdf")
    , ("wiki",   spawn "alacritty -e vim wiki/index.md")
    , ("xm",     spawn "alacritty -e vim .xmonad/xmonad.hs")
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

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift
