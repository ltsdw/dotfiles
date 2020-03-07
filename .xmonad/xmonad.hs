import XMonad
import System.IO(hPutStrLn)
import Data.List(isInfixOf)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- layout
import XMonad.Layout.Named(named)
import XMonad.Layout.Grid
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Minimize(minimize)
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Spacing(spacingRaw, Border(Border))
import XMonad.Layout.Fullscreen(fullscreenSupport, fullscreenFocus)
import XMonad.Layout.NoBorders(noBorders, smartBorders, hasBorder)
import XMonad.Layout.ToggleLayouts

-- hooks
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, defaultPP, wrap, xmobarPP, xmobarColor, shorten, ppOutput, ppCurrent, ppHidden, ppLayout, ppTitle, ppUrgent)
import XMonad.Hooks.EwmhDesktops(ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks(avoidStruts, manageDocks, docks)
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicProperty

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(safeSpawn, spawnPipe)
import XMonad.Util.NamedScratchpad

-- actions
import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.FlexibleResize as Flex

-------------------------------------------------------------------------------

-- Main Function
main = do
        xmproc <- spawnPipe ("xmobar $HOME/.xmobar/xmobarrc")
        xmonad  $ ewmh
                $ docks
                $ fullscreenSupport
                $ withNavigation2DConfig myNav2DConfig
                $ defaultConfig
                    { modMask            = mod4Mask
                    , keys               = myKeys
                    , terminal           = "st"
                    , mouseBindings      = myMouseBindings
                    , layoutHook         = myLayout
                    , manageHook         = myManageHook
                    , handleEventHook    = fullscreenEventHook <+> manageSpotify
                    , startupHook        = myStartupHook
                    , workspaces         = myWorkspaces'
                    , normalBorderColor  = normalBorderColor'
                    , focusedBorderColor = focusedBorderColor'
                    , logHook            = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
                    }

-------------------------------------------------------------------------------

-- Pretty Printing
myPP = xmobarPP
        { ppCurrent = xmobarColor "#00ffe6" "" . wrap "     " "     "
        , ppHidden  = xmobarColor "#6d6d6d" "" . wrap " " " "
        , ppLayout  = xmobarColor "#6d6d6d" ""
        , ppTitle   = xmobarColor "#6d6d6d" "" . shorten 30
        , ppUrgent  = xmobarColor "#ff0000" ""
        }
 
-------------------------------------------------------------------------------

-- Border color
normalBorderColor'  = "#8b8b8b"
focusedBorderColor' = "#00ffcb"

-------------------------------------------------------------------------------

-- Spacing between windows
gaps a b = spacingRaw False (Border a a a a) True (Border b b b b) True

-------------------------------------------------------------------------------

-- Layout
myLayout = toggleLayouts fullscreen myLayoutPerWorkspace
        where
                myLayoutPerWorkspace = onWorkspace "Steam App" smfloat
                                       grid

                fullscreen  = named "FullNB" (minimize $ noBorders (fullscreenFocus Full))
                smfloat     = named "SmpF"   (minimize $ smartBorders simpleFloat)
                grid        = named "GridS"  (minimize $ smartBorders $ avoidStruts $ gaps 1 4 Grid)

-------------------------------------------------------------------------------

-- Navigation Layout
myNav2DConfig = def
        { defaultTiledNavigation = centerNavigation
        , floatNavigation        = centerNavigation
        , layoutNavigation       = [("Full", centerNavigation)]
        , unmappedWindowRect     = [("Full", singleWindowRect)]
        }

-------------------------------------------------------------------------------

-- Workspaces
myWorkspaces = [ (xK_1, "1"), (xK_2, "2"), (xK_3, "3")
               , (xK_n, "Firefox"), (xK_a, "Anki e Estudos")
               , (xK_o, "Origin"), (xK_g, "Steam App"), (xK_4, "Steam")
               ]

myWorkspaces' = (map snd myWorkspaces)

-------------------------------------------------------------------------------

-- Scrathpad
scratchpad = [ NS "Spotify" "spotify" ( className =? "Spotify" ) defaultFloating ]

manageSpotify = dynamicPropertyChange "WM_CLASS" (className =? "Spotify" --> centerFloat)
        where 
                centerFloat  = customFloating $ W.RationalRect (1/10) (1/10) (8/10) (8/10)

-------------------------------------------------------------------------------

-- Like '=?' but matches substrings.
q =?? x = fmap (isInfixOf x) q

-- Rules
myManageHook = composeAll
        [ isFullscreen                                                             --> doFullFloat
        , className  =?  "St"       <&&> title =? "st"                             --> doShift "1"
        , className  =?  "firefox"                                                 --> doShift "Firefox"
        , className  =?  "Anki"                                                    --> doShift "Anki e Estudos"
        , className  =?  "Wine"                                                    --> hasBorder False
        , className  =?  "Steam"                                                   --> doShift "Steam"
        , className  =?  "steam_app_0" <&&> appName =? "bethesda.net_launcher.exe" --> doShift "Steam App" <+> hasBorder False
        , className  =?? "steam_app"                                               --> doShift "Steam App" <+> hasBorder False
        ] <+> namedScratchpadManageHook scratchpad

-------------------------------------------------------------------------------

-- Keybinds
myKeys conf@(XConfig {modMask = modMask}) = M.fromList $

        -- terminal
        [ ((modMask,               xK_t                    ), spawn "st")

        -- dmenu
        , ((modMask,               xK_d                    ), spawn "dmenu_run")

        -- recompile and restart xmonad
        , ((modMask .|. shiftMask, xK_r                    ), spawn "xmonad --recompile && xmonad --restart")

        -- toggle fullscreen
        , ((modMask,               xK_f                    ), sequence_ [ withFocused $ windows . W.sink, sendMessage (Toggle "FullNB")])

        -- make focused window float
        , ((modMask .|. shiftMask, xK_space                ), withFocused $ \w -> floatLocation w >>= windows . W.float w . snd)

        -- put windows back at tile
        , ((modMask,               xK_space                ), withFocused $ windows . W.sink)

        -- minimize and maximize a window
        , ((modMask,               xK_KP_Subtract          ), withFocused minimizeWindow)
        , ((modMask,               xK_KP_Add               ), withLastMinimized maximizeWindowAndFocus)

        -- bright
        , ((0,                     xF86XK_MonBrightnessUp  ), spawn "light -A 2")
        , ((0,                     xF86XK_MonBrightnessDown), spawn "light -U 2")

        -- volume
        , ((0,                     xF86XK_AudioRaiseVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        , ((0,                     xF86XK_AudioLowerVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")

        -- move focus
        , ((modMask,               xK_k                    ), windowGo U False)
        , ((modMask,               xK_h                    ), windowGo L False)
        , ((modMask,               xK_j                    ), windowGo D False)
        , ((modMask,               xK_l                    ), windowGo R False)

        -- move windows
        , ((modMask .|. shiftMask, xK_k                    ), windowSwap U False)
        , ((modMask .|. shiftMask, xK_h                    ), windowSwap L False)
        , ((modMask .|. shiftMask, xK_j                    ), windowSwap D False)
        , ((modMask .|. shiftMask, xK_l                    ), windowSwap R False)

        -- screenshot
        , ((0,                     xK_Print                ), spawn "sleep 0.2 && scrot -q 100 'Screenshot-%d-%m-%Y_%H-%M-%S.png' -e 'mv $f ~/PrintScreens'")
        , ((controlMask,           xK_Print                ), spawn "sleep 0.2 && scrot -s -q 100 -f 'Screenshot-%d-%m-%Y_%H-%M-%S.png' -e 'mv $f ~/PrintScreens'")

        -- scratchpad
        , ((modMask,               xK_p                    ), namedScratchpadAction scratchpad "Spotify")

        -- close focused window
        , ((modMask,               xK_BackSpace            ), kill)

        ]

        -- use mod+keysym to move to the workspace
        ++
        [ ((modMask, key), (windows $ W.greedyView ws))
                | (key,ws) <- myWorkspaces
        ]

        -- use mod+shift+keysym to move a window to the workspace
        ++
        [ ((modMask .|. shiftMask, key), (windows $ W.shift ws))
                | (key,ws) <- myWorkspaces
        ]

-------------------------------------------------------------------------------

-- Mouse Bindings
myMouseBindings (XConfig {modMask = modMask}) = M.fromList $
        -- use mod+shift+button1 to float a window and move by dragging
        [ ((modMask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

        -- resize a window
        , ((modMask .|. shiftMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
        ]

-------------------------------------------------------------------------------

-- StartupHook
myStartupHook = do
        spawnOnce "picom --config ~/.config/picom/compton.conf"
        spawnOnce "feh --no-fehbg --bg-fill ~/.backgrounds/fluttershy.jpg"
        spawnOnce "xsetroot -cursor_name left_ptr"

-------------------------------------------------------------------------------
