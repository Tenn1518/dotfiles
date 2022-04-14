-- Tenn1518's XMonad configuration

import XMonad

import XMonad.Actions.Warp
import XMonad.Actions.Commands
import XMonad.Actions.RotSlaves

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane
import XMonad.Layout.TwoPanePersistent

main :: IO ()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . docks
  . withSB mySB
  $ myConf
  where
    mySB = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

myConf = def
    { modMask            = mod4Mask
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    , terminal           = "alacritty -e zsh -c 'tmux a || tmux'"
    , focusFollowsMouse  = False
    , borderWidth        = 3
    , normalBorderColor  = "#000000"
    , focusedBorderColor = violet
    -- , focusedBorderColor = "#bd93f9"
    }
    `additionalKeysP`
    [ ("M-S-e"     , spawn "emacsclient -c -n -a 'emacs'")
    , ("M-<Space>" , spawn "rofi -modi combi -show combi --combi-modi windowcd,drun")
    , ("M-S-<Space>" , spawn "rofi -modi drun -show drun")
    , ("M-<Tab>"   , sendMessage NextLayout)
    , ("M-S-<Tab>" , sendMessage FirstLayout)
    , ("M-S-;"     , commands >>= runCommand)
      -- Banish or beckon cursor, akin to Stump
    , ("M-S-b"     , banishScreen LowerRight)
    , ("M-b"       , warpToWindow 0.5 0.5)
      -- Rotate slave windows (TODO: don't clobber monitor keybinding)
    , ("M-r"       , rotSlavesUp)
    , ("M-S-r"     , rotSlavesDown)
      -- Gaps
    , ("M-g"       , toggleWindowSpacingEnabled)
    , ("M-["       , decScreenWindowSpacing 2)
    , ("M-]"       , incScreenWindowSpacing 2)
      -- brightness
    , ("<XF86MonBrightnessUp>"  , spawn "brightnessctl s +4%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 4%-")
      -- volume
    , ("<XF86AudioLowerVolume>" , spawn "pamixer -d 4; vol-info")
    , ("<XF86AudioRaiseVolume>" , spawn "pamixer -i 4; vol-info")
    , ("<XF86AudioMute>"        , spawn "pamixer -t; vol-info")
    ]

-- `M-S-;'
commands :: X [(String, X())]
commands = defaultCommands

-- Xmobar info-passing prefs
myXmobarPP :: PP
myXmobarPP = def
  { ppOrder             = \(ws:_:t:_) -> [ws,t] -- Don't print layout
  , ppCurrent            = xmobarColor "#f8f8f2" magenta . pad
  , ppHidden             = pad
  , ppVisible            = pad
  , ppHiddenNoWindows    = pad
  , ppTitleSanitize      = shorten 50
  , ppTitle              = xmobarBorder "Bottom" magenta 2
  , ppSep                = "  "
  }

-- Layouts
myLayoutHook = smartBorders
               $ spacingWithEdge 6 (avoidStruts (myTall
                                                 ||| myTwoPane))
               ||| Full
               where
                 -- Two panes, new windows split slave pane
                 myTall  = Tall nmaster delta ratio
                 -- Two splits, new windows swap into slave pane
                 myTwoPane = TwoPane delta gratio
                 -- parameters
                 nmaster = 1
                 delta   = 3/100
                 ratio   = 1/2
                 gratio  = 56/100

-- If adding more in the future:
-- myManageHook = (otherStuff) <+> (fmap not isDialog --> doF avoidMaster)
myManageHook = fmap not isDialog --> doF avoidMaster

-- Window manager application autostart
myStartupHook :: X ()
myStartupHook = do
  spawn "~/.local/bin/trayer.sh &" -- launches system tray
  spawn "blueman-applet &"                               -- Bluetooth tray icon
  spawn "nm-applet &"                                    -- Network tray icon
  spawn "sh ~/.fehbg"                                    -- background
  spawn "light-locker --lock-on-lid --lock-on-suspend &" -- Screen locker
  spawn "picom &"

-- Windows do not displace master window when it is focused
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c

-- Colors (grabbed from doom-molokai-theme.el)
bg        = "#1c1e1f"
bgalt    = "#222323"
fg        = "#d6d6d4"
fgalt    = "#556172"
white     = fg
grey      = "#525254"
red       = "#e74c3c"
orange    = "#fd971f"
green     = "#b6e63e"
yellow    = "#e2c770"
blue      = "#268bd2"
darkblue = "#727280"
magenta   = "#fb2874"
violet    = "#9c91e4"
cyan      = "#66d9ef"
darkcyan = "#8fa1b3"
