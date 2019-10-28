{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--
-- xmonad config used by Jesse Hallett
-- http://github.com/hallettj/config_files

import qualified Custom.Colors as Colors
import qualified Custom.Prompt.FuzzyWindow as WP
import Data.Default (def)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)
import System.IO
import System.Exit
import XMonad
import qualified XMonad.Actions.Navigation2D as DD
import XMonad.Actions.CycleWS (nextScreen, swapNextScreen, toggleWS')
import qualified XMonad.Actions.DynamicWorkspaces as DWS
import XMonad.Actions.TagWindows (addTag, tagPrompt)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook (NoUrgencyHook(..), clearUrgents, focusUrgent, withUrgencyHook)
import XMonad.Layout.LayoutHints (layoutHintsToCenter)
import XMonad.Layout.MultiToggle (EOT(..), Toggle(..), Transformer, (??), mkToggle, transform)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(MirrorExpand, MirrorShrink), ResizableTall(..))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import qualified XMonad.Prompt as Prompt
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad ( NamedScratchpad(..)
                                   , NamedScratchpads
                                   , customFloating
                                   , namedScratchpadAction
                                   , namedScratchpadFilterOutWorkspacePP
                                   , namedScratchpadManageHook )
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid (Endo, mconcat)
import Control.Applicative ((<$>))
import Control.Monad (filterM, mapM_, sequence, void)

import qualified XMonad.Util.FirefoxScratchpad as TS

------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "/usr/bin/kitty"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces :: [String]
myWorkspaces = ["home"]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "Gimp"           --> doFloat
    , isDialog                      --> doCenterFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]
    <+> namedScratchpadManageHook myScratchPads

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ TS.taggedScratchpad (TS.TS { TS.tag = "primary Firefox", TS.cmd = "firefox", TS.hook = leftPanel 0.67 })
  , slackCommunity "theaustinspace.slack.com"
  , slackCommunity "nycjsorg.slack.com"
  , slackCommunity "olioapps.slack.com"
  , slackCommunity "originate.slack.com"
  , slackCommunity "pdxjs.slack.com"
  , slackCommunity "vantahq.slack.com"
  , NS "google music"    spawnGoogleMusic   findGoogleMusic   (leftPanel  0.67)
  , NS "keybase"         spawnKeybase       findKeybase       (rightPanel 0.67)
  , NS "poodle"          spawnPoodle        findPoodle        (leftPanel 0.67)
  , NS "tox"             spawnTox           findTox           (rightPanel 0.67)
  , NS "gitter"          spawnGitter        findGitter        (rightPanel 0.67)
  , NS "hangouts"        spawnHangouts      findHangouts      (rightPanel 0.67)
  , NS "whatsapp"        spawnWhatsapp      findWhatsapp      (rightPanel 0.67)
  , NS "rememberthemilk" spawnRTM           findRTM           (rightPanel 0.67)
  , NS "signal"          spawnSignal        findSignal        (rightPanel 0.67)
  , NS "enpass"          spawnEnpass        findEnpass        (leftPanel 0.67)
  ]
  where
    slackCommunity domain = NS
      domain
      (chromeApp ("https://" ++ domain ++ "/"))
      (resource =? domain)
      (rightPanel 0.67)

    spawnGoogleMusic = chromeApp "https://play.google.com/music"
    findGoogleMusic = resource =? "play.google.com__music"

    spawnKeybase = undefined
    findKeybase = className =? "Keybase"

    spawnPoodle = "poodle"
    findPoodle = title =? "Poodle"

    spawnTox = "qtox"
    findTox = className =? "qTox"

    spawnGitter = "/opt/Gitter/linux64/Gitter"
    findGitter = title =? "Gitter"

    spawnHangouts = chromeApp' "knipolnnllmklapflnccelgolnpehhpl"
    findHangouts = title =? "Hangouts - hallettj@gmail.com"

    spawnWhatsapp = chromeApp "https://web.whatsapp.com/"
    findWhatsapp = resource =? "web.whatsapp.com"

    spawnRTM = "'/opt/Remember The Milk/rememberthemilk'"
    findRTM = className =? "Remember The Milk"

    spawnSignal = "/opt/Signal/signal-desktop"
    findSignal = className =? "Signal"

    spawnEnpass = "/opt/enpass/Enpass"
    findEnpass = title =? "Enpass"

    chromeApp  = (("google-chrome --user-data-dir=" ++ dataDir ++ " --app=") ++)
    chromeApp' = (("google-chrome --user-data-dir=" ++ dataDir ++ " --app-id=") ++)
    dataDir   = "$HOME/.config/google-chrome-apps"

    rightPanel w = customFloating $ W.RationalRect l t w h
      where
        h = 1
        t = 1 - h
        l = 1 - w

    leftPanel w = customFloating $ W.RationalRect l t w h
      where
        h = 1
        t = 1 - h
        l = 0

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = layoutHintsToCenter             $
           smartBorders                    $
           mkToggle (SFULL ?? FULL ?? EOT) $
           avoidStruts                     $
           split ||| ThreeColMid 1 (1/60) (20/60)

split = renamed [Replace "tall"] $
        ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio   = 30/60
    delta   = 1/60

data MyTransformers = SFULL
    deriving (Read, Show, Eq, Typeable)

instance Transformer MyTransformers Window where
    transform SFULL x k = k (avoidStruts Full) (const x)

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the solarized theme.
--

myNormalBorderColor  = Colors.base00
myFocusedBorderColor = Colors.orange

-- Color of current window title in xmobar.
xmobarTitleColor = Colors.orange

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = Colors.yellow

xmobarUrgentFG = Colors.base03
xmobarUrgentBG = Colors.yellow

-- Width of the window border in pixels.
myBorderWidth = 3

myFont = "xft:monospace:size=12:antialias=true"

------------------------------------------------------------------------
-- Prompt style
--

promptConfig :: Prompt.XPConfig
promptConfig = def { Prompt.font            = myFont
                   , Prompt.bgColor         = Colors.base03
                   , Prompt.fgColor         = Colors.base0
                   , Prompt.bgHLight        = Colors.base0
                   , Prompt.fgHLight        = Colors.base03
                   , Prompt.borderColor     = myNormalBorderColor
                   , Prompt.height          = 48
                   , Prompt.alwaysHighlight = True
                   }

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod1Mask

myKeys :: XConfig a -> [(String, X())]
myKeys conf =
  [ ("M-, t", spawn $ XMonad.terminal conf)
  , ("M-q", restart "xmonad" True)
  , ("M-S-q", io exitSuccess)
  , ("M-S-x", spawn "dm-tool switch-to-greeter")

  , ("M-f", spawn "xfce4-appfinder")

  , ("M-<Backspace>", focusUrgent)
  , ("M-S-<Backspace>", clearUrgents)

  , ("M-, p", namedScratchpadAction myScratchPads "poodle")
  , ("M-, m", namedScratchpadAction myScratchPads "google music")
  , ("M-, a", namedScratchpadAction myScratchPads "theaustinspace.slack.com")
  , ("M-, n", namedScratchpadAction myScratchPads "nycjsorg.slack.com")
  , ("M-, o", namedScratchpadAction myScratchPads "olioapps.slack.com")
  , ("M-, c", namedScratchpadAction myScratchPads "originate.slack.com")
  , ("M-, v", namedScratchpadAction myScratchPads "vantahq.slack.com")
  , ("M-, x", namedScratchpadAction myScratchPads "tox")
  , ("M-, g", namedScratchpadAction myScratchPads "gitter")
  , ("M-, h", namedScratchpadAction myScratchPads "hangouts")
  , ("M-, w", namedScratchpadAction myScratchPads "whatsapp")
  , ("M-, l", namedScratchpadAction myScratchPads "rememberthemilk")
  , ("M-, s", namedScratchpadAction myScratchPads "signal")
  , ("M-, e", namedScratchpadAction myScratchPads "enpass")
  , ("M-, k", namedScratchpadAction myScratchPads "keybase")
  , ("M-p", spawn "/opt/enpass/Enpass showassistant")

  -- Arranging windows

  , ("M-<Return>", windows W.focusMaster)
  , ("M-S-<Return>", windows W.swapMaster)

  , ("M-S-z", sendMessage (Toggle SFULL))
  -- , ("M-<F11>", sendMessage (Toggle FULL))
  , ("M-C-b", sendMessage (Toggle FULL))

  , ("M-a",   sendMessage MirrorShrink)
  , ("M-;",   sendMessage MirrorExpand)

  , ("M-z",   toggleWS' ["NSP"])

  , ("M-C-w", kill)  -- close focused window

  -- -- Increment the number of windows in the master area.
  -- , ("M-l",   sendMessage $ IncMasterN 1)
  -- -- Decrement the number of windows in the master area.
  -- , ("M-s",   sendMessage $ IncMasterN (-1))

  , ("M-s", nextScreen)
  , ("M-S-s", swapNextScreen)

  -- dynamic workspaces
  , ("M-w", DWS.selectWorkspace promptConfig)
  , ("M-C-S-w", DWS.removeWorkspace)

  -- prompts
  , ("M-g", WP.fuzzyWindowPrompt promptConfig WP.Goto WP.matchTitle WP.allWindows)
  , ("M-b", WP.fuzzyWindowPrompt promptConfig WP.Bring WP.matchTitleAndTags WP.allWindows)
  , ("M-S-w", DWS.withWorkspace promptConfig (\ws -> windows (W.shift ws)))
  , ("M-, S-t", tagPrompt promptConfig (\s -> withFocused (addTag s)))
  ]

vicfryzelKeys conf@(XConfig {modMask}) = M.fromList $
  -- Lock the screen using xscreensaver.
  [ ((modMask .|. controlMask, xK_l),
     spawn "xset s activate")

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_p),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn "screenshot")

  -- Mute volume.
  , ((0, 0x1008ff12),
     void toggleMute)

  -- Decrease volume.
  , ((0, 0x1008ff11),
     volume "-1%" >>= alert . ("Volume: " ++) . show)
  , ((modMask, xK_Page_Down),
     volume "-1%" >>= alert . ("Volume: " ++) . show)

  -- Increase volume.
  , ((0, 0x1008ff13),
     volume "+1%" >>= alert . ("Volume: " ++) . show)
  , ((modMask, xK_Page_Up),
     volume "+1%" >>= alert . ("Volume: " ++) . show)

  -- Audio previous.
  , ((0, 0x1008FF16),
     prevTrack)
  , ((modMask, xK_Home),
     prevTrack)

  -- Play/pause.
  , ((0, 0x1008FF14),
     playPause)
  , ((modMask, xK_Insert),
     playPause)

  -- Audio next.
  , ((0, 0x1008FF17),
     nextTrack)
  , ((modMask, xK_End),
     nextTrack)

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  -- Adjust brightness
  , ((0, xF86XK_MonBrightnessUp),
     spawn "xbacklight +1")
  , ((0, xF86XK_MonBrightnessDown),
     spawn "xbacklight -1")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)
  , ((modMask .|. mod5Mask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask .|. shiftMask, xK_Tab),
     windows W.focusUp)
  , ((modMask .|. shiftMask .|. mod5Mask, xK_Tab),
     windows W.focusUp)

  -- Push window back into tiling.
  , ((modMask .|. controlMask, xK_t),
     withFocused $ windows . W.sink)
  ]

------------------------------------------------------------------------
-- Mouse bindings
--

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     \w -> focus w >> mouseMoveWindow w)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       \w -> focus w >> windows W.swapMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       \w -> focus w >> mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP {
    ppOutput  = hPutStrLn h
  , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 100
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppVisible = shorten 100  -- no special formatting
  , ppSep     = "   "
  , ppUrgent  = xmobarColor xmobarUrgentFG xmobarUrgentBG
}


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
  setWMName "LG3D"
  spawn "bash ~/.Xsession"


------------------------------------------------------------------------
-- Run xmonad
--
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ DD.navigation2DP def
                       ("k", "h", "j", "l")
                       [("M-", DD.windowGo),
                        ("M-S-", DD.windowSwap)]
                       False
    $ def {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = False,
      clickJustFocuses   = False,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = \c -> vicfryzelKeys c `M.union` mkKeymap c (myKeys c),
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      handleEventHook    = mconcat [ handleEventHook def
                                   , fullscreenEventHook
                                   -- , makeFirefoxFullScreen (return True)
                                   ],
      logHook            = myLogHook xmproc,
      startupHook        = myStartupHook
      }


------------------------------------------------------------------------
-- Helpers
--
alert :: (Show a) => a -> X ()
alert a = spawn $ "echo \"" ++ show a ++ "\" > ~/.config/statnot/notification.pipe"

volume :: (MonadIO m, Functor m) => String -> m Int
volume change = read <$> runProcessWithInput "volume" [change] ""

toggleMute :: (MonadIO m) => m ()
toggleMute = spawn "amixer -D pulse sset Master toggle"

playPause :: X ()
playPause = sendToMusicPlayer "space"

nextTrack :: X ()
nextTrack = sendToMusicPlayer "Right"

prevTrack :: X ()
prevTrack = sendToMusicPlayer "Left"

sendToMusicPlayer :: String -> X ()
sendToMusicPlayer key =
    withActiveNamedScratchpad (sendKeyPress key) confs players
  where
    confs   = myScratchPads
    players = ["pandora", "google music"]

withActiveNamedScratchpad :: (Window -> X ())
                          -> NamedScratchpads
                          -> [String]
                          -> X ()
withActiveNamedScratchpad f confs names =
    catMaybes <$> sequence pads >>= mapM_ f
  where
    pads    = findActiveNamedScratchpad confs <$> names

sendKeyPress :: String -> Window -> X ()
sendKeyPress key win = spawn $ "xdotool key --window " ++ show win ++ " " ++ key

-- generalized from someNamedScratchpadAction in XMonad.Util.NamedScratchpad
findActiveNamedScratchpad :: NamedScratchpads
                          -> String
                          -> X (Maybe Window)
findActiveNamedScratchpad confs n
  | Just conf <- findByName confs n = withWindowSet $ \s -> do
                   filterCurrent <- filterM (runQuery (query conf))
                                      ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)
                   filterAll <- filterM (runQuery (query conf)) (W.allWindows s)
                   return $ listToMaybe $ filterCurrent ++ filterAll
  | otherwise = return Nothing

findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = listToMaybe $ filter ((s==) . name) c
