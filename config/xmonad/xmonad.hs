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
import XMonad.Hooks.DynamicLog

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane
import XMonad.Layout.TwoPanePersistent

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . docks . withSB mySB $ myConf
  where
    mySB = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

myConf = def
    { modMask            = mod4Mask
    , terminal           = "alacritty -e zsh -c 'tmux a || tmux'"
    , focusFollowsMouse  = False
    , borderWidth        = 3
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#bd93f9"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , logHook            = dynamicLog
    }
    `additionalKeysP`
    [ ("M-S-e"     , spawn "emacsclient -c -n -a 'emacs'")
    , ("M-<Space>" , spawn "rofi -modi windowcd,run -show combi --combi-modi windowcd,drun")
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

commands :: X [(String, X())]
commands = defaultCommands

-- sending XMonad state to XMobar
myXmobarPP :: PP
myXmobarPP            = def
  { ppLayout          = const ""
  , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 3
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppTitle           = shorten 50
  , ppSep             = " · "
  }
  where
    white, lowWhite :: String -> String
    white             = xmobarColor "#f8f8f2" ""
    lowWhite          = xmobarColor "#bbbbbb" ""

myLayoutHook = smartBorders
               $ avoidStruts
               $ spacingWithEdge 3 (myTall
                                    ||| myTwoPane)
               ||| Full
               where
                 -- Two panes, new windows split slave pane
                 myTall  = Tall nmaster delta ratio
                 -- Two splits, new windows swap into slave pane
                 myTwoPane = TwoPane delta gratio
                 nmaster = 1
                 delta   = 3/100
                 ratio   = 1/2
                 gratio  = 56/100

-- If adding more in the future:
-- myManageHook = (otherStuff) <+> (fmap not isDialog --> doF avoidMaster)
myManageHook = fmap not isDialog --> doF avoidMaster

-- Windows do not displace master window when it is focused
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c
