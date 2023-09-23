import qualified Data.Map as M
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions (NamedAction)
import Data.Map (Map)
import XMonad.Util.SpawnOnce (spawnOnce)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.DynamicLog (xmobarProp)
import System.Directory.Internal.Prelude (getEnv)
import System.Environment (getEnvironment)
import GHC.IO (unsafePerformIO)

-- Increase brightness: xbacklight -inc 5
-- Decrease brightness: xbacklight -dec 5
-- Get brightness: xbacklight -get

-- Increase sound: amixer set Master 5%+
-- Decrease sound: amixer set Master 5%-
-- Toggle mute sound: amixer set Master toggle

-- Battery: cat /sys/class/power_supply/BAT0/capacity

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
  , ((mod1Mask, xK_Return), spawn "wezterm") -- TODO: myTerminal
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+") , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
  ]

myStartupHook :: X ()
myStartupHook = do
  -- Otherwise, xmonad still uses X cursor by default
  spawn "xsetroot -cursor_name left_ptr"
  spawnOnce setWallpaper

myConfig myTerminal = def
  { terminal = myTerminal
  --, keys = myKeys TODO: Use myKeys and not additionalKeys
  , startupHook = myStartupHook
  }
  `additionalKeys`
  myKeys0

main :: IO()
main = do
  -- Get terminal from environment
  myTerminal <- getEnv "TERMINAL"
  -- Make xmonad EWMH compliant with proper fullscreen (Extended Window Manager Hints)
  -- https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html
  xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig myTerminal
