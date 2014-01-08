-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
--
-- customized by Jesse Hallett
-- http://github.com/hallettj/config_files

import Graphics.X11.ExtraTypes.XF86 (xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CycleWS (nextScreen, swapNextScreen, toggleWS')
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.MarkAsUrgent (markAsUrgent)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook (UrgencyHook, NoUrgencyHook(..), clearUrgents, focusUrgent, urgencyHook, withUrgencyHook)
import XMonad.Layout.Flip (Flip(..))
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.ResizableTile (MirrorResize(MirrorExpand, MirrorShrink), ResizableTall(..))
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Dzen (addArgs, center, dzenConfig, font, onCurr)
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad ( NamedScratchpad(NS)
                                   , customFloating
                                   , namedScratchpadAction
                                   , namedScratchpadFilterOutWorkspacePP
                                   , namedScratchpadManageHook )
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map        as M
import Data.Monoid (All (All), mappend)
import Control.Applicative ((<$>))
import Control.Monad ((>=>), when)


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/urxvt"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:web","2:work","3:comms","4:terminal","5:vim"] ++ map show [6..9]


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
myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat  -- bigpicture-mode
    , className =? "Gimp"           --> doFloat
    , className =? "Pidgin"         --> doF (W.shift "3:chat") <+> markAsUrgent
    , className =? "stalonetray"    --> doIgnore
    , isDialog                      --> doCenterFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]
    <+> namedScratchpadManageHook myScratchPads

myScratchPads = [ NS "pandora"      spawnPandora     findPandora     (rightPanel 0.50)
                , NS "rdio"         spawnRdio        findRdio        (rightPanel 0.67)
                , NS "google music" spawnGoogleMusic findGoogleMusic (rightPanel 0.67)
                , NS "pidgin"       spawnPidgin      findPidgin      (rightPanel 0.25)
                ]
  where
    spawnPandora = chromeApp "http://www.pandora.com/"
    findPandora = resource =? "www.pandora.com"

    spawnRdio = chromeApp "http://www.rdio.com/"
    findRdio = resource =? "www.rdio.com"

    spawnGoogleMusic = chromeApp "https://play.google.com/music"
    findGoogleMusic = resource =? "play.google.com__music"

    spawnPidgin = "pidgin"
    findPidgin  = role =? "buddy_list"

    chromeApp = (("google-chrome --user-data-dir=" ++ dataDir ++ " --app=") ++)
    dataDir   = "$HOME/.config/google-chrome-apps"

    rightPanel w = customFloating $ W.RationalRect l t w h
      where
        h = 1
        t = 1 - h
        l = 1 - w

    role = stringProperty "WM_WINDOW_ROLE"

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
myLayout = avoidStruts (
    onWorkspaces ["1:web", "4:terminal", "7", "8"] leftTiled tiled |||
    tiled |||
    leftTiled |||
    Full) |||
    noBorders (fullscreenFull Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []

    -- like tiled, but puts the master window on the right
    leftTiled = Flip tiled

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 3/4

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the solarized theme.
--

base03 = "#002b36"
base02 = "#073642"
base01 = "#586e75"
base00 = "#657b83"
base0  = "#839496"
base1  = "#93a1a1"
base2  = "#eee8d5"
base3  = "#fdf6e3"
yellow = "#b58900"
orange = "#cb4b16"
red    = "#dc322f"
magenta= "#d33682"
violet = "#6c71c4"
blue   = "#268bd2"
cyan   = "#2aa198"
green  = "#859900"

myNormalBorderColor  = base00
myFocusedBorderColor = orange

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor   = orange,
    activeTextColor     = base0,
    activeColor         = base03,
    inactiveBorderColor = base00,
    inactiveTextColor   = base02,
    inactiveColor       = base03
}

-- Color of current window title in xmobar.
xmobarTitleColor = orange

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = yellow

xmobarUrgentFG = base03
xmobarUrgentBG = yellow

-- Width of the window border in pixels.
myBorderWidth = 1


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod1Mask

myKeys :: [(String, X())]
myKeys =
  [ ("M-<Backspace>", focusUrgent)
  , ("M-S-<Backspace>", clearUrgents)

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),
  --, ("M-b", sendMessage ToggleGaps)
  , ("M-a",   sendMessage MirrorShrink)
  , ("M-;",   sendMessage MirrorExpand)

  , ("M-s",   nextScreen)
  , ("M-S-s", swapNextScreen)
  , ("M-z",   toggleWS' ["NSP"])

  , ("M-, p", namedScratchpadAction myScratchPads "pandora")
  , ("M-, r", namedScratchpadAction myScratchPads "rdio")
  , ("M-, m", namedScratchpadAction myScratchPads "google music")
  , ("M-, b", namedScratchpadAction myScratchPads "pidgin")
  , ("M-, ,", sendMessage (IncMasterN (-1))) ]

vicfryzelKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Launch dmenu via yeganesh.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn "exe=`dmenu-with-yeganesh` && eval \"exec $exe\"")

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
     toggleMute >> return ())

  -- Decrease volume.
  , ((0, 0x1008ff11),
     volume "1%-" >>= alert . ("Volume: " ++) . show)

  -- Increase volume.
  , ((0, 0x1008ff13),
     volume "1%+" >>= alert . ("Volume: " ++) . show)

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  -- Adjust brightness
  , ((0, xF86XK_MonBrightnessUp),
     spawn "xbacklight +10")
  , ((0, xF86XK_MonBrightnessDown),
     spawn "xbacklight -10")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

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

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  -- Disabled because it conflicts with my <leader> shortcuts.
  -- , ((modMask, xK_comma),
  --    sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

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
  setWMName "LG3D" -- Improves compatibility with Java applications.


------------------------------------------------------------------------
-- Event Hooks:
--
myHandleEventHook = handleEventHook defaultConfig `mappend` fullscreenEventHook


------------------------------------------------------------------------
-- Urgency Hooks:
--
-- data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

-- instance UrgencyHook LibNotifyUrgencyHook where
--     urgencyHook LibNotifyUrgencyHook w = do
--         name <- getName w
--         ws   <- gets windowset
--         c    <- withDisplay $ \d -> fmap resClass $ io $ getClassHint d w
--         whenJust (W.findTag w ws) (flash name c)
--       where
--         flash _ "Pidgin" _  = spawn "true"
--         flash _ "emesene" _ = spawn "true"
--         flash name c index  = spawn $
--                               intercalate " " $
--                               [ "notify-send -i"
--                               , icon
--                               , show $ show name
--                               , show $ "on " ++ index ]
--           where icon = case c of
--                          "URxvt"   -> "gnome-terminal"
--                          otherwise -> map toLower c


------------------------------------------------------------------------
-- Run xmonad
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = \c -> vicfryzelKeys c `M.union` mkKeymap c myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myHandleEventHook,
    logHook            = myLogHook xmproc,
    startupHook        = myStartupHook
  }


------------------------------------------------------------------------
-- Helpers
--
volume :: (MonadIO m, Functor m) => String -> m Int
volume change = read <$> runProcessWithInput "volume" [change] ""

toggleMute :: (MonadIO m) => m ()
toggleMute = spawn "amixer -D pulse sset Master toggle"

alert :: (Show a) => a -> X ()
alert a = spawn $ "echo \"" ++ (show a) ++ "\" > ~/.config/statnot/notification.pipe"
