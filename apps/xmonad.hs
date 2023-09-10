import XMonad
import XMonad.Util.EZConfig

main = xmonad $ def { terminal = "wezterm" }
       `additionalKeys`
       [ ((mod1Mask, xK_p), spawn "rofi -show drun")
       ]
       
