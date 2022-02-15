import XMonad

import XMonad.Actions.Warp
import XMonad.Actions.Commands

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

mySB = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure xmobarPP)
main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . docks . withSB mySB $ myConf

myConf = def
    { modMask            = mod4Mask
    , terminal           = "alacritty -e zsh -c 'tmux a || tmux'"
    , focusFollowsMouse  = False
    , borderWidth        = 1
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#cccccc"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    }
    `additionalKeysP`
    [ ("M-S-e", spawn "emacsclient -c -n -a 'emacs'")
    , ("M-<Space>", spawn "rofi -modi windowcd,run -show combi --combi-modi windowcd,drun")
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-<Tab>", sendMessage FirstLayout)
    , ("M-S-;", commands >>= runCommand)
      -- Banish or beckon cursor, akin to Stump
    , ("M-S-b", banishScreen LowerRight)
    , ("M-b", warpToWindow 0.5 0.5)
      -- Gaps
    , ("M-g", toggleWindowSpacingEnabled)
    , ("M-[", decScreenWindowSpacing 2)
    , ("M-]", incScreenWindowSpacing 2)
      -- volume
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 4; vol-info")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 4; vol-info")
    , ("<XF86AudioMute>", spawn "pamixer -t; vol-info")
    ]

commands :: X [(String, X())]
commands = defaultCommands

myLayoutHook = smartBorders $ avoidStruts $
               spacingWithEdge 3 (myTall |||
                                   Mirror (myTall)) |||
               Full
               where
                 myTall  = Tall nmaster delta ratio
                 nmaster = 1
                 delta   = 3/100
                 ratio   = 1/2

-- If adding more in the future:
-- myManageHook = (otherStuff) <+> (fmap not isDialog --> doF avoidMaster)
myManageHook = fmap not isDialog --> doF avoidMaster

-- Windows do not displace master window when it is focused
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c
