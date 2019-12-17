import XMonad
import System.IO
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- layout
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle.Instances

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(safeSpawn, spawnPipe)

-- action
import XMonad.Actions.SpawnOn

-------------------------------------------------------------------------------

-- Main Function
main = do
        xmproc <- spawnPipe ("xmobar $HOME/.xmobarrc/xmobarrc")
        xmonad $ ewmh $ docks $ fullscreenSupport $ defaultConfig {
                  modMask = mod4Mask
                , keys = myKeys
                , layoutHook = myLayout
                , manageHook = myManageHook
                , startupHook = myStartupHook
                , workspaces = myWorkspaces'
                , logHook = dynamicLogWithPP $ defaultPP {ppOutput = hPutStrLn xmproc}}

-------------------------------------------------------------------------------

-- Layout
myLayout = myLayoutPerWorkspace $ toggleLayouts fullscreen grid
        where
            fullscreen = named "FullNB" (noBorders (fullscreenFull Full))
            grid = named "GridSS" (smartBorders $ avoidStruts $ smartSpacing 5 Grid )

            myLayoutPerWorkspace = onWorkspace "1" $ toggleLayouts fullscreen grid2
                    where
                        grid2 = named "GridS" (smartBorders $ avoidStruts $ spacing 5 Grid)

-- Workspaces
myWorkspaces = [  (xK_1, "1"), (xK_2, "2"), (xK_3, "3"), (xK_4, "4"), (xK_5, "5")
                , (xK_6, "6"), (xK_7, "7"), (xK_8, "8"), (xK_9, "9")
                , (xK_n, "Firefox"), (xK_a, "Anki e Estudos")
                , (xK_c, "League of Legends Client"), (xK_g, "League of Legends Game")  ]

myWorkspaces' = (map snd myWorkspaces)

-------------------------------------------------------------------------------

-- Rules
myManageHook = composeAll
        [ isFullscreen                                                      --> doFullFloat
        , className =? "St"                                                 --> doShift "1"
        , className =? "firefox"                                            --> (hasBorder False <+> doShift "Firefox")
        , className =? "Anki"                                               --> doShift "Anki e Estudos"
        , (className =? "Wine" <&&> appName =? "leagueclientux.exe")        --> (hasBorder False <+> doShift "League of Legends Client")
        , (className =? "Wine" <&&> appName =? "league of legends.exe")     --> doShift "League of Legends Game" ]

-------------------------------------------------------------------------------

-- Keybinds
myKeys conf@(XConfig {modMask = modMask}) = M.fromList $
 
	-- terminal
        [ ((modMask,               xK_t                    ), spawn "st")

        -- dmenu
        , ((modMask,               xK_d                    ), spawn "dmenu_run")

        -- restart xmonad
        , ((modMask .|. shiftMask, xK_r                    ), spawn "xmonad --recompile && xmonad --restart")

        -- Toggle fullscreen
        , ((modMask,               xK_f                    ), sendMessage (Toggle "FullNB"))

        -- Bright
        , ((0,                     xF86XK_MonBrightnessUp  ), spawn "xbacklight +5")
        , ((0,                     xF86XK_MonBrightnessDown), spawn "xbacklight -5")

        -- Volume
        , ((0,                     xF86XK_AudioRaiseVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        , ((0,                     xF86XK_AudioLowerVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")

        -- Killing focused window
        , ((modMask,               xK_BackSpace          ), kill) ]

        -- use mod+keysym to move to the workspace
        ++ [ ((modMask, key), (windows $ W.greedyView ws))
                | (key,ws) <- myWorkspaces ] 

        -- use mod+shift+keysym to move a window to the workspace
        ++ [ ((modMask .|. shiftMask, key), (windows $ W.shift ws))
                | (key,ws) <- myWorkspaces ]

-------------------------------------------------------------------------------

-- StartHook
myStartupHook = do
        spawn "picom --config $HOME/.config/picom/compton.conf"
        spawnOnce "feh --no-fehbg --bg-fill /usr/share/backgrounds/Riven-Picture.png"

-------------------------------------------------------------------------------
