module Topics
  ( myTopics
  , myTopicConfig)
where
import XMonad
import Themes
import qualified Data.Map as M
import           XMonad.Actions.TopicSpace
import           XMonad.StackSet as W

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
              >> spawn "alacritty --working-directory uni -e ranger")
    , ("wiki",   spawn "alacritty -e vim wiki/index.md")
    , ("xm",     spawn "alacritty -e vim .xmonad/xmonad.hs")
    ]
  }