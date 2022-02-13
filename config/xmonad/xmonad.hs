import XMonad

import XMonad.Actions.Warp
import XMonad.Actions.Commands

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Hooks.EwmhDesktops

main :: IO ()
main = xmonad $ myConf

myConf = def
    { modMask            = mod4Mask
    , terminal           = "alacritty -e zsh -c 'tmux a || tmux'"
    , focusFollowsMouse  = False
    , borderWidth        = 1
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#cccccc"
    }
    `additionalKeysP`
    [ ("M-S-e", spawn "emacsclient -c -n -a 'emacs'")
    , ("M-<Space>", spawn "rofi -modi windowcd,run -show combi --combi-modi windowcd,drun")
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-;", commands >>= runCommand)
        -- Banish or beckon cursor, akin to Stump
    , ("M-S-b", banishScreen LowerRight)
    , ("M-b", warpToWindow 0.5 0.5)
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 4; vol-info")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 4; vol-info")
    , ("<XF86AudioMute>", spawn "pamixer -t; vol-info")
    ]

commands :: X [(String, X())]
commands = defaultCommands
