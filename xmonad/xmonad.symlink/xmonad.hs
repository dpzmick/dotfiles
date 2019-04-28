import System.IO
import XMonad
import XMonad.Config (def)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.ThreeColumns

myLayout = ThreeColMid 1 (3/100) (1/2)

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/dpzmick/.xmobarrc"
    xmonad $ ewmh def
        { terminal = "gnome-terminal"

        -- something about logging through xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#2aa198" "" . shorten 50
                        , ppCurrent = \s -> xmobarColor "#b58900" "" ("["++s++"]")
                        }

        , manageHook = manageDocks <+> manageHook def

        , layoutHook = avoidStruts (layoutHook def ||| myLayout)

        , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook

        -- colors
        , borderWidth        = 3
        , focusedBorderColor = "#d33682" -- 'magenta' from solarized
        , normalBorderColor  = "#073642" -- 'base02' from solarized
        }

        `additionalKeysP`
        [ ("<XF86MonBrightnessDown>" , spawn "xbacklight -1")
        , ("<XF86MonBrightnessUp>"   , spawn "xbacklight +1")
        , ("<XF86AudioRaiseVolume>"  , spawn "amixer -D pulse sset Master 5%+")
        , ("<XF86AudioLowerVolume>"  , spawn "amixer -D pulse sset Master 5%-")
        , ("<Print>"                 , spawn "sleep 0.2; take-screenshot.sh")

        , ("M-p"                     , spawn "dmenu_run -fn 'xft:Ubuntu Mono:size=7:antialias=true' -nb '#002B36' -nf '#93a1a1' -sb '#93a1a1' -sf '#002b36'")
        , ("M-b", sendMessage ToggleStruts)

        -- app launchers
        , ("M-<Print>"               , spawn "google-chrome")
        , ("C-S-<Return>"              , spawn "gnome-terminal -- ssh -Y worf.local")
        ]
