{-# LANGUAGE BlockArguments #-}

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map as M

main :: IO ()
main = do
  spawn "picom"
  -- spawn "feh --bg-fill ~/.xmonad/pentium4.jpg"
  spawn "xset r rate 200 40"
  xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

-- | Representation of numbers in an FR keyboard (0 <= i <= 9)
frNum :: Int -> Char
frNum x = "à&é\"'(-è_ç" !! x

myConfig = def
  { modMask    = mod4Mask      -- Rebind Mod to the Super key
  , layoutHook = myLayout      -- Use custom layouts
  , manageHook = myManageHook  -- Match on certain windows
  }
  `removeKeysP`
  [ "M-h", "M-j", "M-k", "M-l", "M-S-c" ]
  `additionalKeysP`
  [ ("M-S-f", spawn "firefox")
  , ("M-S-t", spawn "alacritty")
  , ("M-<F4>", kill)
  , ("M-<Up>", windows W.focusUp)
  , ("M-<Down>", windows W.focusDown)
  , ("M-S-<Up>", windows W.swapUp)
  , ("M-S-<Down>", windows W.swapDown)
  , ("M-<Left>", sendMessage Shrink)
  , ("M-<Right>", sendMessage Expand)
  , ("M-<F2>", spawn "pamixer -d 5")
  , ("M-<F3>", spawn "pamixer -i 5")
  , ("M-<F5>", spawn "brightnessctl s 10%-")
  , ("M-<F6>", spawn "brightnessctl s +10%")
  , ("M-f", toggleFull)] ++
  [ ("M-S-"++[frNum i], windows $ W.greedyView (show i)) | i <- [1..9] ] ++
  [ ("M-"++[frNum i], windows $ W.shift (show i)) | i <- [1..9] ]

-- | Toggle full screen
toggleFull = withFocused \ windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats
  then do
    withFocused $ toggleBorder
    withFocused $ windows . W.sink
  else do
    withFocused $ toggleBorder
    -- we must leave some space for the status bar (=> 0.015)
    withFocused $ windows . (flip W.float $ W.RationalRect 0 0.015 1 1)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = mySpacing $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

mySpacing =
  spacingRaw
    True              -- False=apply envn when single window
    (Border 3 3 3 3)  -- Screen borders for top bot right left
    True              -- enable screen borders
    (Border 3 3 3 3)  -- window border size
    True              -- enable window borders

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
