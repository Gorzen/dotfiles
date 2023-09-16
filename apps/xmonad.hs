import XMonad
import XMonad.Util.EZConfig

-- Increase brightness: xbacklight -inc 5
-- Decrease brightness: xbacklight -dec 5
-- Get brightness: xbacklight -get

-- Increase sound: amixer set Master 5%+
-- Decrease sound: amixer set Master 5%-
-- Toggle mute sound: amixer set Master toggle

-- Battery: cat /sys/class/power_supply/BAT0/capacity


main = xmonad $ def { terminal = "wezterm" }
       `additionalKeys`
       [ ((mod1Mask, xK_p), spawn "rofi -show drun")
       ]
       
