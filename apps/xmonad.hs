import qualified Data.Map as M
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions (NamedAction)
import Data.Map (Map)
import XMonad.Util.SpawnOnce (spawnOnce)
import Graphics.X11.ExtraTypes.XF86

-- Increase brightness: xbacklight -inc 5
-- Decrease brightness: xbacklight -dec 5
-- Get brightness: xbacklight -get

-- Increase sound: amixer set Master 5%+
-- Decrease sound: amixer set Master 5%-
-- Toggle mute sound: amixer set Master toggle

-- Battery: cat /sys/class/power_supply/BAT0/capacity

myTerminal :: String
myTerminal = "wezterm"

-- TODO: Use xmonad's way to set wallpaper?
setWallpaper :: String
setWallpaper = "feh --bg-scale --random $HOME/Images/Wallpapers"

-- TODO: Use
--myKeys :: XConfig l0 -> Map (ButtonMask, KeySym) (X ())
--myKeys c = M.fromList
-- [ ... ]

myKeys0 :: [((KeyMask, KeySym), X ())]
myKeys0 =
  [ ((mod1Mask, xK_p), spawn "rofi -show drun")
  , ((mod1Mask, xK_w), spawn setWallpaper)
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+") , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
  ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr" -- For some reason, this is needed for XMonad to use the defined default cursor theme. Otherwise, XMonad still uses the X cursor
  spawn setWallpaper

main = xmonad $ def
  { terminal = myTerminal
  --, keys = myKeys TODO: Use myKeys and not additionalKeys
  , startupHook = myStartupHook
  }
  `additionalKeys`
  myKeys0

