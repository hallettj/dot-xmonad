{-# LANGUAGE NamedFieldPuns #-}

module XMonad.Util.FirefoxScratchpad
  ( TaggedScratchpad(..)
  , taggedScratchpad
  , findTagged
  ) where

import Control.Monad.Reader (ask)
import XMonad
import qualified XMonad.Util.NamedScratchpad as N
import qualified XMonad.Actions.TagWindows as Tag

data TaggedScratchpad = TS
  { tag  :: String
  , cmd  :: String
  , hook :: ManageHook
  }

taggedScratchpad :: TaggedScratchpad -> N.NamedScratchpad
taggedScratchpad TS { tag, cmd, hook } = N.NS
  { N.name  = tag
  , N.cmd   = cmd
  , N.query = findTagged tag
  , N.hook  = applyTag tag <+> hook
  }

findTagged :: String -> Query Bool
findTagged t = do
  win <- ask  -- iterate over windows that have matched query so far
  liftX $ Tag.hasTag t win

applyTag :: String -> ManageHook
applyTag tag = do
  win <- ask
  liftX $ Tag.addTag tag win
  doF id




-- isFirefox :: Query Bool
-- isFirefox = className =? "Firefox"
