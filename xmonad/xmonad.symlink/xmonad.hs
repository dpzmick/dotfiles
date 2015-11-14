import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(unsafeSpawn)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops


main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/dpzmick/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = "gnome-terminal"

        -- something about logging through xmobar
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        -- something about fullscreen in chrome
        , handleEventHook = fullscreenEventHook

        -- colors
        , borderWidth        = 1
        , focusedBorderColor = "#d33682" -- 'magenta' from solarized
        , normalBorderColor  = "#073642" -- 'base02' from solarized
        }
        `additionalKeysP`
        [ ("<XF86MonBrightnessDown>" , spawn "xbacklight -10")
        , ("<XF86MonBrightnessUp>"   , spawn "xbacklight +10")
        , ("<XF86AudioRaiseVolume>"  , spawn "amixer -D pulse sset Master 5%+")
        , ("<XF86AudioLowerVolume>"  , spawn "amixer -D pulse sset Master 5%-")
        , ("<Print>"                 , spawn "sleep 0.2; take-screenshot.sh")

        -- app launchers
        , ("M-<Print>"               , spawn "google-chrome")
        ]
