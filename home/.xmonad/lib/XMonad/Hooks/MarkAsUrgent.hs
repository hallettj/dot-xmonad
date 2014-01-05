module XMonad.Hooks.MarkAsUrgent (markAsUrgent) where

import XMonad
import Foreign.C.Types (CLong)

realUrgencyHintBit :: CLong
realUrgencyHintBit = fromIntegral urgencyHintBit

markAsUrgent :: ManageHook
markAsUrgent = do
  w <- ask
  liftX $ withDisplay $ \d -> io $ do
    h <- getWMHints d w
    setWMHints d w (h { wmh_flags = wmh_flags h .|. realUrgencyHintBit })
  idHook
