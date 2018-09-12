-----------------------------------------------------------------------------
-- |
-- Module      :  Custom.Prompt.FuzzyWindow
-- Copyright   :  Devin Mullins <me@twifkak.com>
--                Andrea Rossato <andrea.rossato@unibz.it>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jesse Hallett <jesse@sitr.us>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A variation of XMonad.Prompt.Window with fuzzy matching in the prompt.
-- xprompt operations to bring windows to you, and bring you to windows.
--
----------------------------------------------------------------------------

module Custom.Prompt.FuzzyWindow
  ( module XMonad.Prompt.Window
  , fuzzyWindowPrompt
  , matchTags
  , matchTitle
  , matchTitleAndTags
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intersect, sortOn)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Monoid.Textual (TextualMonoid)
import qualified Text.Fuzzy as Fuzzy
import XMonad
import XMonad.Actions.CopyWindow (copyWindow)
import qualified XMonad.Actions.TagWindows as T
import XMonad.Actions.WindowBringer (bringWindow)
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W

-- | Pops open a prompt with window titles belonging to
-- winmap. Choose one, and an action is applied on the
-- selected window, according to WindowPrompt.
fuzzyWindowPrompt :: Prompt.XPConfig -> WindowPrompt -> (M.Map String Window -> X (String -> IO [String])) -> XWindowMap -> X ()
fuzzyWindowPrompt c t promptBy winmap = do
  a <- case t of
    Goto          -> fmap gotoAction  winmap
    Bring         -> fmap bringAction winmap
    BringCopy     -> fmap bringCopyAction winmap
    BringToMaster -> fmap bringToMaster winmap
  wm <- winmap
  complFunc <- promptBy wm
  Prompt.mkXPrompt t c complFunc a

    where
      winAction a m   = flip whenJust (windows . a) . flip M.lookup m
      gotoAction      = winAction W.focusWindow
      bringAction     = winAction bringWindow
      bringCopyAction = winAction bringCopyWindow
      bringToMaster   = winAction (\w s -> W.shiftMaster . W.focusWindow w $ bringWindow w s)

matchTitle :: M.Map String Window -> X (String -> IO [String])
matchTitle wm = return $ \winTitle -> return . fuzzyMatches winTitle . map fst . M.toList $ wm

matchTags :: M.Map String Window -> X (String -> IO [String])
matchTags wm = do
  tagAssocs <- mapM (\(winTitle, win) -> (,) <$> pure winTitle <*> T.getTags win) (M.toList wm)
  let tags = tagAssocs >>= snd
  return $ \tag -> let
    matchingTags = fuzzyMatches tag tags
    matchingAssocs = filter (\(_, winTags) -> not $ null $ winTags `intersect` matchingTags) tagAssocs
    in return $ map fst matchingAssocs

matchTitleAndTags :: M.Map String Window -> X (String -> IO [String])
matchTitleAndTags wm = do
  byTitle <- matchTitle wm
  byTags <- matchTags wm
  return $ \q -> (<>) <$> byTitle q <*> byTags q

fuzzyMatches :: TextualMonoid s => s -> [s] -> [s]
fuzzyMatches query options = map Fuzzy.original sorted
  where
    filtered = Fuzzy.filter query options mempty mempty id False
    sorted   = sortOn (negate . Fuzzy.score) filtered

-- | Brings a copy of the specified window into the current workspace.
bringCopyWindow :: Window -> WindowSet -> WindowSet
bringCopyWindow w ws = copyWindow w (W.currentTag ws) ws
