import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(unsafeSpawn)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus

myManageHook = manageDocks <+> manageHook defaultConfig
myLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2

myStartupHook = setWMName "LG3D" -- deek

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/dpzmick/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = "gnome-terminal"

        -- something about logging through xmobar
        -- , manageHook = manageDocks <+> manageHook defaultConfig
        , manageHook = myManageHook
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#2aa198" "" . shorten 50
                        , ppCurrent = \s -> xmobarColor "#b58900" "" ("["++s++"]")
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

        , ("M-b", sendMessage ToggleStruts)

        -- app launchers
        , ("M-<Print>"               , spawn "google-chrome")
        ]
