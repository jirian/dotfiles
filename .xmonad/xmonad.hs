-- Defaults
import System.Exit
import Data.Monoid
import qualified Data.Map as Map

-- Core
import XMonad
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

-- Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

-- Util
import qualified XMonad.Util.Cursor as Cursor

-- X11
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault


-- VARIABLES
defaultTerminal :: String
defaultTerminal = "st"

defaultWorkspaces :: [String]
defaultWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

scriptDir :: String
scriptDir = "~/.local/bin/"


-- KEYBINDINGS
defaultKeyBindings conf@(XConfig {XMonad.modMask = modm}) = Map.fromList $
    -- Launch terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- Launch rofi
    , ((modm, xK_p), spawn "rofi -show run")

    -- Close focused window
    , ((modm, xK_q), kill)

    -- Next layout algorithm
    , ((modm, xK_space), sendMessage NextLayout)

    -- Default layout
    , ((modm .|. shiftMask, xK_Tab), setLayout $ XMonad.layoutHook conf)

    -- Reset window size
    , ((modm, xK_n), refresh)

    -- Moving focus
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)

    -- Moving windows
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)

    -- Resizing the master area
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm, xK_Print), sendMessage $ IncMasterN 1)
    , ((modm, xK_Pause), sendMessage $ IncMasterN (-1))

    -- Tile window back
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Toggle status bar gap
    , ((modm, xK_x), sendMessage ToggleStruts)

    -- Volume keys
    , ((noModMask, xF86XK_AudioLowerVolume), lowerVolume)
    , ((modm, xK_KP_Enter), lowerVolume)
    --
    , ((noModMask, xF86XK_AudioRaiseVolume), raiseVolume)
    , ((modm, xK_KP_Add), raiseVolume)

    -- Multimedia keys
    , ((noModMask, xF86XK_AudioPlay), playPause)
    , ((modm, xK_KP_Multiply), playPause)
    --
    , ((noModMask, xF86XK_AudioPrev), prevTrack)
    , ((modm, xK_KP_Divide), prevTrack)
    --
    , ((noModMask, xF86XK_AudioNext), nextTrack)
    , ((modm, xK_KP_Subtract), nextTrack)

    -- Monitor brightness
    , ((noModMask, xF86XK_MonBrightnessDown), spawn $ scriptDir ++ "brightness -d 100")
    , ((noModMask, xF86XK_MonBrightnessUp), spawn $ scriptDir ++ "brightness -i 100")

    -- Quit XMonad
    , ((modm .|. shiftMask, xK_Caps_Lock), io $ exitWith ExitSuccess)

    -- Restart XMonad
    , ((modm, xK_Caps_Lock), spawn "xmonad --recompile && xmonad --restart")
    ]
    
    ++
    -- Switching and moving windows to workspaces
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf)
            [ xK_plus
            , xK_ecaron
            , xK_scaron
            , xK_ccaron
            , xK_rcaron
            , xK_zcaron
            , xK_yacute
            , xK_aacute
            , xK_iacute
            , xK_eacute
            ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ] ++ [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

    ++
    -- Switching and moving windows to screens
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    where
        lowerVolume = spawn "pamixer -d 5 --allow-boost"
        raiseVolume = spawn "pamixer -i 5 --allow-boost"
        playPause = spawn "playerctl play-pause"
        prevTrack = spawn "playerctl previous"
        nextTrack = spawn "playerctl next"


-- MOUSEBINDINGS
defaultMouseBindings (XConfig {XMonad.modMask = modm}) = Map.fromList $
    [ ( (modm, button1)
      , (\w -> focus w
        >> mouseMoveWindow w
        >> windows W.shiftMaster
      ))
    , ( (modm, button3)
      , (\w -> focus w
        >> mouseResizeWindow w
        >> windows W.shiftMaster
      ))
    ]

-- LAYOUT
evenSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
evenSpacing = spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True

myLayoutHook = tall
    ||| full
    where
    	tall = renamed [Replace "Tall"]
            $ evenSpacing
            $ Tall 1 (3/100) (1/2)

        full = renamed [Replace "Full"]
            $ noBorders
            $ Full


-- MANAGE HOOK
myManageHook = composeAll
    [ ]


-- EVENT HOOK
myEventHook = mempty


-- LOG HOOK
myLogHook = return ()


-- STARTUP HOOK
myStartupHook = do
    Cursor.setDefaultCursor Cursor.xC_left_ptr
    setWMName "LG3D"


-- MAIN
main :: IO ()
main = xmonad $ docks $ ewmh $ defaultConfig
    { modMask = mod4Mask
    , terminal = defaultTerminal
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 2
    , workspaces = defaultWorkspaces
    , normalBorderColor = "#282828"
    , focusedBorderColor = "#d5c4a1"
    , keys = defaultKeyBindings
    , mouseBindings = defaultMouseBindings
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook
    , startupHook = myStartupHook
    }
