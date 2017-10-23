{-# LANGUAGE NamedFieldPuns #-}

module Custom.Queries
  ( isOnCurrentWorkspace
  , isOnWorkspace
  ) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import XMonad (Query, WorkspaceId, XState(..), liftX)
import XMonad.StackSet (current, findTag, tag, workspace)

isOnWorkspace :: WorkspaceId -> Query Bool
isOnWorkspace workspaceId = do
  window <- ask
  XState { windowset } <- liftX get
  case findTag window windowset of
    Just id -> return (id == workspaceId)
    Nothing -> return False

isOnCurrentWorkspace :: Query Bool
isOnCurrentWorkspace = do
  window <- ask
  XState { windowset } <- liftX get
  let currentWorkspaceId = tag $ workspace $ current windowset
  case findTag window windowset of
    Just id -> return (id == currentWorkspaceId)
    Nothing -> return False
