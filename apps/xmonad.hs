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
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp)
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

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
  spawnOnce "trayer --edge top --align right --SetDockType true \
            \--SetPartialStrut true --expand true --width 10 \
            \--transparent true --tint 0x5f5f5f --height 18"
  spawnOnce "nm-applet"
  spawnOnce "blueman-applet"

-- Keybinding to toggle xmobar
myToggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
myToggleStrutsKey _ = (mod1Mask, xK_B)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myConfig myTerminal = def
  { modMask = mod1Mask
  , terminal = myTerminal
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
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) myToggleStrutsKey
    $ myConfig myTerminal
