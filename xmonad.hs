import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)

toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myConfig = desktopConfig
  { modMask = mod4Mask
  , terminal = "urxvt"
    -- For status bar:
  , layoutHook = avoidStruts $ layoutHook defaultConfig
  , manageHook = manageHook defaultConfig <+> manageDocks
  } `additionalKeys`
  [ ((mod4Mask .|. controlMask, xK_space), spawn "rofi -show run")
  ]

main = xmonad =<< statusBar "xmobar" xmobarPP toggleStrutsKey myConfig
