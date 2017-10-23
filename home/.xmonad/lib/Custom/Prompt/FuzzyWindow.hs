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
  ) where

import Data.List (sortOn)
import qualified Data.Map as M
import Data.Monoid.Textual (TextualMonoid)
import qualified Text.Fuzzy as Fuzzy
import XMonad
import XMonad.Actions.CopyWindow (copyWindow)
import XMonad.Actions.WindowBringer (bringWindow)
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W

-- | Pops open a prompt with window titles belonging to
-- winmap. Choose one, and an action is applied on the
-- selected window, according to WindowPrompt.
fuzzyWindowPrompt :: Prompt.XPConfig -> WindowPrompt -> XWindowMap -> X ()
fuzzyWindowPrompt c t winmap = do
  a <- case t of
    Goto          -> fmap gotoAction  winmap
    Bring         -> fmap bringAction winmap
    BringCopy     -> fmap bringCopyAction winmap
    BringToMaster -> fmap bringToMaster winmap
  wm <- winmap
  Prompt.mkXPrompt t c (compList wm) a

    where
      winAction a m   = flip whenJust (windows . a) . flip M.lookup m
      gotoAction      = winAction W.focusWindow
      bringAction     = winAction bringWindow
      bringCopyAction = winAction bringCopyWindow
      bringToMaster   = winAction (\w s -> W.shiftMaster . W.focusWindow w $ bringWindow w s)

      compList m s = return . fuzzyMatches s . map fst . M.toList $ m

fuzzyMatches :: TextualMonoid s => s -> [s] -> [s]
fuzzyMatches query options = map Fuzzy.original sorted
  where
    filtered = Fuzzy.filter query options mempty mempty id False
    sorted   = sortOn (negate . Fuzzy.score) filtered

-- | Brings a copy of the specified window into the current workspace.
bringCopyWindow :: Window -> WindowSet -> WindowSet
bringCopyWindow w ws = copyWindow w (W.currentTag ws) ws
