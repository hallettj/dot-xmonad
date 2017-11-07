{-# LANGUAGE NamedFieldPuns #-}

--------------------------------------------------------------------------------
-- |
-- Module     : Custom.Hooks.MakeFirefoxFullScreen
--
-- Maintainer : Jesse Hallett <jesse@sitr.us>
--
-- As of version 57 Firefox leaves full screen when its window is unmapped
-- (which happens when leaving the workspace that holds the window). I use Tree
-- Style Tabs and Vimium, and I want Firefox to be in full screen mode all of
-- the time. This module exports an event hook that sends an F11 keypress to
-- Firefox windows when they receive a `MapNotifyEvent` to get windows to return
-- to full screen.
--------------------------------------------------------------------------------
module Custom.Hooks.MakeFirefoxFullScreen (makeFirefoxFullScreen) where

import Control.Monad (when)
import Control.Monad.State.Class (get, put)
import Data.Monoid (All(..))
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Extras (Event(..))
import XMonad.Core (Query, X, runQuery, spawn)
import XMonad.ManageHook ((=?), (<&&>), appName, className)
import XMonad.Util.WindowState (runStateQuery)

data FullScreenState = FullScreen | Normal
  deriving (Eq, Read, Show)

-- |
-- The event hook takes a `query` argument to narrow down window selection to
-- Firefox windows that also match the given query.
makeFirefoxFullScreen :: Query Bool -> Event -> X All
makeFirefoxFullScreen query (MapNotifyEvent { ev_window }) = do
  matchQuery query ev_window engageFullScreen
  return mempty
makeFirefoxFullScreen query (UnmapEvent { ev_window }) = do
  matchQuery query ev_window $ setFullScreenState Normal
  return mempty
makeFirefoxFullScreen _ _ = return mempty

matchQuery :: Query Bool -> Window -> (Window -> X ()) -> X ()
matchQuery query win f = do
  let fullQuery = (className =? "Firefox") <&&> (not <$> appName =? "Popup") <&&> query
  matchesQuery <- runQuery fullQuery win
  when matchesQuery $ f win

engageFullScreen :: Window -> X ()
engageFullScreen win = do
  s <- getFullScreenState win
  when (s /= FullScreen) $ do
    setFullScreenState FullScreen win
    sendKeyPress "F11" win

getFullScreenState :: Window -> X FullScreenState
getFullScreenState win = do
  r <- runStateQuery get win :: X (Maybe FullScreenState)
  case r of
    Just s -> return s
    Nothing -> return Normal

setFullScreenState :: FullScreenState -> Window -> X ()
setFullScreenState s win = runStateQuery (put (Just s)) win

sendKeyPress :: String -> Window -> X ()
sendKeyPress key win =
  spawn $ "sleep 0.2; xdotool key --window " ++ show win ++ " " ++ key
