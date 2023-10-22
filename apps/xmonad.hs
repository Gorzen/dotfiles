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
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp, defToggleStrutsKey)
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.Circle (Circle(Circle))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeCol, ThreeColMid))
import XMonad.Layout.Tabbed (TabbedDecoration(Tabbed), simpleTabbed)

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
  , ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+") , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
  ]

-- Put some of these in modules for XDG Autostart?
myStartupHook :: X ()
myStartupHook = do
  -- Start applications following the XDG Autostart specification (nm-applet, blueman-applet, picom, polkit agent, ...)
  spawnOnce "dex -a"
  -- Set cursor to left pointer. Otherwise, xmonad uses the X cursor by default
  spawn "xsetroot -cursor_name left_ptr"
  spawnOnce setWallpaper
  spawnOnce "enable_screen-saver"
  spawn "killall trayer"
  spawn "sleep 2 && trayer --edge top --align right --SetDockType true \
            \--expand true --height 22 --width 6 --iconspacing 8 \
            \--tint 0x282c34 --transparent true --alpha 0 \
            \--distancefrom right --distance 700" -- xmobar size has to be stable (TODO: configure this with nix)


myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog --> doCenterFloat
  , className =? ".blueman-manager-wrapped" --> doCenterFloat
  , className =? "Pavucontrol" --> doCenterFloat
  ]


-- Keybinding to toggle spacing for xmobar
-- TODO: Use me! For some reason, doesn't work (try easy config for mod keybindings?)
myToggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
myToggleStrutsKey _ = (mod1Mask, xK_B)

-- TODO: Add icon root to xmobar
myLayoutPrinter :: String -> String
myLayoutPrinter "Tall" = "<icon=/home/lulu/Documents/Icons-Xmonad/Untitled.xbm/>  Tall"
myLayoutPrinter x = x


--myLayoutHook = smartBorders
--  . mkToggle (NOBORDERS ?? FULL ?? EOT)
--  $ Tall 1 1/2 3/100 ||| Mirror (Tall 1 1/2 3/100) ||| Full

myLayout =
  smartBorders -- Don't show borders if only 1 window or full screen
  . mkToggle (FULL ?? NOBORDERS ?? EOT)
  $ tiled ||| threeCol ||| threeColMid ||| simpleTabbed
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1      -- Default number of windows in the master pane
      ratio   = 1/2    -- Default proportion of screen occupied by master pane
      delta   = 3/100  -- Percent of screen to increment by when resizing panes

      threeCol = ThreeCol arg1 arg2 arg3
      threeColMid = ThreeColMid arg1 arg2 arg3
      arg1 = nmaster -- how many windows initially appear in the main window.
      arg2 = delta -- the amount to resize while resizing
      arg3 = ratio -- the initial size of the columns

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta "  •  "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    , ppLayout          = blue . myLayoutPrinter
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
  , manageHook = myManageHook
  , layoutHook = myLayout
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
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig myTerminal
