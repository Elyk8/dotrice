{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Hooks Imports
-- {{{ IMPORTS
-- Generic Imports
import           Colors.Colors
import           Control.Monad
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Monoid
import           Data.Ratio                                         -- Require for rational (%) operator
import           Data.Semigroup
import           Foreign.C                      ( CInt )
import           System.Directory
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( Handle
                                                , hPutStrLn
                                                , hPutStr
                                                , hClose
                                                )

-- Actions Imports
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.MouseResize
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens ( onNextNeighbour )
import           XMonad.Actions.RotSlaves       ( rotAllDown
                                                , rotSlavesDown
                                                )
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowGo        ( runOrRaise )
import           XMonad.Actions.WithAll         ( killAll
                                                , sinkAll
                                                )

-- Hooks Imports
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..)
                                                , avoidStruts
                                                , docks
                                                , docksEventHook
                                                , manageDocks
                                                )
import           XMonad.Hooks.ManageHelpers     ( (-?>)
                                                , Side(CE, NW)
                                                , (^?)
                                                , composeOne
                                                , doCenterFloat
                                                , doFullFloat
                                                , doSideFloat
                                                , isDialog
                                                , isFullscreen
                                                , isInProperty
                                                , transience'
                                                )
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.RefocusLast       ( isFloat
                                                , refocusLastLayoutHook
                                                , refocusLastLogHook
                                                , refocusLastWhen
                                                , refocusingIsActive
                                                )
import           XMonad.Hooks.Rescreen
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Hooks.StatusBar         ( StatusBarConfig
                                                , dynamicEasySBs
                                                , dynamicSBs
                                                , statusBarProp
                                                , statusBarPropTo
                                                , withSB
                                                )
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.TaffybarPagerHints
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

-- Layout Imports
import           XMonad.Layout.GridVariants     ( Grid(Grid) )
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows     ( decreaseLimit
                                                , increaseLimit
                                                , limitWindows
                                                )
import qualified XMonad.Layout.Magnifier       as Mag
import           XMonad.Layout.MultiToggle      ( (??)
                                                , EOT(EOT)
                                                , mkToggle
                                                , single
                                                )
import qualified XMonad.Layout.MultiToggle     as MT
                                                ( Toggle(..)
                                                , Transformer
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers(MIRROR, NBFULL, NOBORDERS) )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( ToggleLayout(Toggle)
                                                , toggleLayouts
                                                )
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.TwoPanePersistent
import           XMonad.Layout.WindowArranger   ( WindowArrangerMsg(..)
                                                , windowArrange
                                                )
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt                  ( XPPosition(Bottom) )
import qualified XMonad.StackSet               as W

-- Util Imports
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import qualified XMonad.Util.Hacks             as Hacks
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(..)
                                                , NamedScratchpads
                                                , customFloating
                                                , namedScratchpadAction
                                                , namedScratchpadFilterOutWorkspacePP
                                                , namedScratchpadManageHook
                                                , nsHideOnFocusLoss
                                                , scratchpadWorkspaceTag
                                                )
import           XMonad.Util.Run                ( runProcessWithInput
                                                , safeSpawn
                                                , spawnPipe
                                                )
import           XMonad.Util.Scratchpad         ( scratchpadManageHook )
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties
import           XMonad.Util.WorkspaceCompare
-- }}}

-- {{{ LOCAL VARIABLES
myFont :: String
myFont = "xft:monospace:regular:size=11:antialias=true:hinting=true"

myBoldFont = "xft:monospace:weight=bold:size=50"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myTerminalClass :: String
myTerminalClass = "Alacritty"

myTerminalScratch :: String
myTerminalScratch = "alacritty --class " -- Set to open the terminal in the working directory

myEmacs :: String
myEmacs = "emacsclient -cne " -- Makes emacs keybindings easier to type

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalColor :: String
myNormalColor = basebg

myFocusColor :: String
myFocusColor = base05

myLeader :: String
myLeader = "M-<Space>"

windowCount :: X (Maybe String)
windowCount =
  gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myGaps :: Integer
myGaps = 8
-- }}}

-- {{{ STARTUP
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "~/.config/xmonad/scripts/trayer-launch.sh"
-- }}}

-- {{{ MULTI-MONITOR
rescreenCfg = def { afterRescreenHook = myAfterRescreenHook, randrChangeHook = myRandrChangeHook }

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "~/.fehbg"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change --force"

-- isOnScreen :: ScreenId -> WindowSpace -> Bool
-- isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

workspaceOnCurrentScreen :: WSType
workspaceOnCurrentScreen = WSIs $ do
  s <- currentScreen
  -- return $ \x -> W.tag x /= "NSP" && isOnScreen s x -- This is for independent screens
  return $ \x -> W.tag x /= "NSP"

-- spacesOnCurrentScreen :: WSType
-- spacesOnCurrentScreen = WSIs (isOnScreen <$> currentScreen)
-- }}}

-- {{{ THEME
myTabTheme = def { activeColor = base08
                 , inactiveColor = basebg
                 , urgentColor = base12
                 , activeBorderColor = base08
                 , inactiveBorderColor = basebg
                 , urgentBorderColor = base04
                 , activeTextColor = base10
                 , inactiveTextColor = base13
                 , urgentTextColor = base04
                 , fontName = myFont
                 , decoHeight = 30
                 }

myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def { swn_font = myBoldFont, swn_fade = 0.6, swn_bgcolor = basebg, swn_color = base05 }

-- }}}

-- {{{ XMOBAR
myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
  pure $ statusBarPropTo
    ("_XMONAD_LOG_" ++ show s)
    ("xmobar -B \""
    ++ basebg
    ++ "\" -F \""
    ++ basefg
    ++ "\" -x "
    ++ show s
    ++ " ~/.config/xmobar/xmobar"
    ++ show s
    ++ ".hs"
    )
    (pure $ myXmobarPP (S s))

myXmobarPP :: ScreenId -> PP
myXmobarPP s = filterOutWsPP [scratchpadWorkspaceTag] $ def
  { ppSep = "<fc=" ++ base08 ++ "> <fn=1>|</fn> </fc>"
  , ppWsSep = ""
  , ppCurrent = xmobarColor basefg "" . clickable wsIconFull
  , ppVisible = xmobarColor base05 "" . clickable wsIconHidden
  , ppVisibleNoWindows = Just (xmobarColor base05 "" . clickable wsIconEmpty)
  , ppHidden = xmobarColor basefg "" . clickable wsIconHidden
  , ppHiddenNoWindows = xmobarColor basefg "" . clickable wsIconEmpty
  , ppUrgent = xmobarColor base05 "" . (wrap "!" "!" . clickable wsIconFull)
  , ppOrder = \(ws : _ : _ : extras) -> ws : extras
  , ppExtras = [ windowCount
               , wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix
               $ wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix
               $ wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix
               $ layoutColorIsActive s (logLayoutOnScreen s)
               , wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>" ++ "<fn=4>")
                       ("</fn>" ++ actionSuffix)
                 $ titleColorIsActive s (shortenL 50 $ logTitleOnScreen s)
               ]
  }
 where
  wsIconFull = "<fn=2>\xf518 </fn>"
  wsIconHidden = "<fn=2>\xf7e6 </fn>"
  wsIconEmpty = "<fn=2>\xf02d </fn>"
  titleColorIsActive n l = do
    c <- withWindowSet $ return . W.screen . W.current
    if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l
  layoutColorIsActive n l = do
    c <- withWindowSet $ return . W.screen . W.current
    if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l
-- }}}

-- {{{ WORKSPACES
myWorkspaces :: [[Char]]
myWorkspaces = ["ter", "work", "www", "dis", "msg"]

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

-- clickable ws = "<action=xdotool key super+alt+" ++ show i ++ ">" ++ ws ++ "</action>"
--   where i = fromJust $ M.lookup ws myWorkspaceIndices

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+alt+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x : xs) ws = addActions
  xs
  (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
 where
  k = fst x
  b = snd x

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws = addActions [(show i, 1), ("q", 2), ("Left", 4), ("Right", 5)] icon
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myFilter = filterOutWs [scratchpadWorkspaceTag]

-- }}}

-- {{{ LAYOUTS
tall =
  renamed [Replace "tall"]
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ Mag.magnifierOff
    $ mkToggle (single MIRROR)
    $ limitWindows 5
    $ mySpacing myGaps
    $ ResizableTall 1 (3 / 100) (1 / 2) []

wide =
  renamed [Replace "wide"]
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ Mag.magnifierOff
    $ Mirror
    $ limitWindows 5
    $ mySpacing myGaps
    $ ResizableTall 1 (3 / 100) (1 / 2) []

-- monocle =
--   renamed [Replace "monocle"]
--     $ smartBorders
--     $ addTabs shrinkText myTabTheme
--     $ subLayout [] (smartBorders Simplest)
--     $ limitWindows 20 Full

grid =
  renamed [Replace "grid"]
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing myGaps
    $ Mag.magnifierOff
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

spirals =
  renamed [Replace "fibonacci"]
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ Mag.magnifierOff
    $ mkToggle (single MIRROR)
    $ mySpacing myGaps
    $ spiral (6 / 7)

three =
  renamed [Replace "three"]
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 7
    $ Mag.magnifierOff
    $ mkToggle (single MIRROR)
    $ mySpacing myGaps
    $ ThreeColMid 1 (3 / 100) (1 / 2)

floats =
  renamed [Replace "floats"] $ Mag.magnifierOff $ smartBorders $ limitWindows 20 simplestFloat

deck =
  renamed [Replace "deck"]
    $ smartBorders
    $ Mag.magnifierOff
    $ mkToggle (single MIRROR)
    $ mySpacing myGaps
    $ TwoPanePersistent Nothing (3 / 100) (1 / 2)

tabs = renamed [Replace "tabs"] $ tabbed shrinkText myTabTheme

-- accordion = renamed [Replace "accordion"] $ mkToggle (single MIRROR) Accordion

myLayoutHook =
  refocusLastLayoutHook
    . trackFloating
    $ avoidStruts
    $ mouseResize
    $ windowArrange
    $ windowNavigation
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout =
    withBorder myBorderWidth tall
      ||| wide
      ||| noBorders tabs
      ||| grid
      ||| deck
      ||| spirals
      ||| three

-- }}}

-- {{{ WINDOW RULES
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [ title =? t --> doIgnore | t <- myIgnores ]
      , [ className =? c --> doShift (head myWorkspaces) | c <- myW1C ]
      , [ className =? c --> doShift (myWorkspaces !! 1) | c <- myW2C ]
      , [ className =? c --> doShift (myWorkspaces !! 2) | c <- myW3C ]
      , [ className =? c --> doShift (myWorkspaces !! 3) | c <- myW4C ]
      , [ className =? c --> doShift (myWorkspaces !! 4) | c <- myW5C ]
      , [ className =? c --> doFloat | c <- myFloatC ]
      , [ className =? c --> doCenterFloat | c <- myFloatCC ]
      , [ name =? n --> doSideFloat NW | n <- myFloatSN ]
      , [ name =? n --> doF W.focusDown | n <- myFocusDC ]
      , [role =? "LINE" --> doFloat]
      , [role =? "GtkFileChooserDialog" --> doFloat]
      , [role =? "pop-up" --> doCenterFloat]
      , [iconName =? "Gvim        " --> doFloat]
      , [iconName =? "Launch Application" --> doFloat]
      , [isFullscreen --> doFullFloat]
      , [isDialog --> doFloat]
      ]
 where
  name = stringProperty "WM_NAME"
  role = stringProperty "WM_WINDOW_ROLE"
  iconName = stringProperty "WM_ICON_NAME"
  myIgnores = ["SafeEyes-0", "SafeEyes-1"]
  myW1C = ["VSCodium"]
  myW2C = ["Chromium", "Zotero"]
  myW3C = ["Brave-browser", "qutebrowser", "librewolf", "firefox"]
  myW4C = ["discord", "zoom"]
  myW5C = ["VirtualBox Manager", "VirtualBox Machine", "Thunderbird"]
  myFloatC = ["confirm", "file_progress", "dialog", "download", "error", "toolbar", "Gmrun"]
  myFloatCC = ["notification", "Yad", "Xfce4-power-manager-settings", "Dragon-drop"]
  myFloatSN = ["Event Tester"]
  myFocusDC = ["Event Tester", "Notify-osd"]

myHandleEventHook :: Event -> X All
myHandleEventHook =
  swallowEventHook (className =? myTerminalClass) (return True)
    <+> refocusLastWhen isFloat
    <+> Hacks.trayerAboveXmobarEventHook
    <+> Hacks.windowedFullscreenFixEventHook
    <+> serverModeEventHookCmd

-- {{{ SCRATCHPADS
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "ncmpcpp" launchNcmpcpp (appName =? "ncmpcpp") (customFloating $ W.RationalRect l t w h)
  , NS "terminal" launchTerminal (appName =? "scratchpad") (customFloating $ W.RationalRect l t w h)
  , NS "pulsemixer"
       launchPulsemixer
       (appName =? "pulsemixer")
       (customFloating $ W.RationalRect l t w h)
  ]
 where
  launchNcmpcpp = myTerminalScratch ++ "ncmpcpp -e ncmpcpp"
  launchTerminal = myTerminalScratch ++ "scratchpad"
  launchPulsemixer = myTerminalScratch ++ "pulsemixer -e pulsemixer"
  h = 0.8
  w = 0.6
  t = (1 - h) / 2
  l = (1 - w) / 2

-- }}}

-- {{{ SUBMAPPINGS
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe $ "yad --text-info --fontname=\"monospace\" --width 800 --height 600 --center --title \"XMonad keybindings\""
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- {{{ KEYBINDINGS
myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  [ 
    -- Xmonad
    ("M-C-r", addName "Restart XMonad" $ spawn "xmonad --restart; killall xmobar")
  , ("M-S-<Esc>", addName "Quit XMonad" $ io exitSuccess)

    -- Kill windows
  , ("M-q", addName "Kill Client" $ kill1) -- Kill selected client
  , ("M-S-q", addName "Kill All on current Workspace" $ killAll) -- Kill all windows on current workspace

    -- Sticky Windows
  , ("M-v", addName "Copy windows to all workspaces" $ windows copyToAll) -- Make focused window always visible in all workspaces
  , ("M-S-v", addName "Kill all other copies" $ killAllOtherCopies) -- Toggle window state back

    -- Floating windows
  , ("M-g", addName "Toggle Float layout" $ sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
  , ("M-t", addName "Toggle Tiling/Float" $ withFocused toggleFloat) -- Toggle focused window floating/tiled
  , ("M-C-s", addName "For all floating windows to tile" $ sinkAll) -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
  , ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4) -- Decrease window spacing
  , ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4) -- Increase window spacing
  , ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4) -- Decrease screen spacing
  , ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4) -- Increase screen spacing
  , ("C-M1-m", addName "Toggle gaps" $ toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled) -- Toggle gaps

    -- Windows navigation
  , ("M-m", addName "Move focus to master" $ windows W.focusMaster) -- Move focus to the master window
  , ("M-j", addName "Focus next" $ windows W.focusDown) -- Move focus to the next window
  , ("M-k", addName "Focus prev" $ windows W.focusUp) -- Move focus to the prev window
  , ("M-S-m", addName "Swap focused and master" $ windows W.swapMaster) -- Swap the focused window and the master window
  , ("M-S-j", addName "Swap focused with next" $ windows W.swapDown) -- Swap focused window with next window
  , ("M-S-k", addName "Swap focused with previous" $ windows W.swapUp) -- Swap focused window with prev window
  , ("M-<Backspace>", addName "Dwm promote" $ whenX (swapHybrid True) dwmpromote) -- Swap master window and last swapped window or first window in stack
  , ("M-S-<Tab>", addName "Rotate all windows in stack" $ rotSlavesDown) -- Rotate all windows except master and keep focus in place
  , ("M-C-<Tab>", addName "Rotate all windows" $ rotAllDown) -- Rotate all the windows in the current stack
  , ("M-`", addName "Swap view on monitors" $ onNextNeighbour def W.greedyView)

    -- Screen navigation
  , ("M-w", addName "Change focussed monitor" $ onNextNeighbour def W.view)

    -- Layouts
  , ("M-b", addName "Switch to next layout" $ sendMessage NextLayout) -- Switch to next layout
  , ("M-f", addName "Toggles full layout" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-r", addName "Zoom focused client" $ sendMessage Mag.Toggle) -- Zoom focused client

    -- Increase/decrease windows in the master pane or the stack
  , ("M-S-<Up>", addName "Increase no. master" $ sendMessage (IncMasterN 1)) -- Increase # of clients master pane
  , ("M-S-<Down>", addName "Decrease no. master" $ sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
  , ("M-C-<Up>", addName "Increase windows limit" $ increaseLimit) -- Increase # of windows
  , ("M-C-<Down>", addName "Decrease windows limit" $ decreaseLimit) -- Decrease # of windows

    -- Window resizing
  , ("M-h", addName "Shrink horiz window width" $ sendMessage Shrink) -- Shrink horiz window width
  , ("M-l", addName "Expand horiz window width" $ sendMessage Expand) -- Expand horiz window width
  , ("M-M1-j", addName "Shrink vert window width" $ sendMessage MirrorShrink) -- Shrink vert window width
  , ("M-M1-k", addName "Expand vert window width" $ sendMessage MirrorExpand) -- Expand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
  , ("M-C-h", addName "pullGroup L"           $ sendMessage $ pullGroup L)
  , ("M-C-l", addName "pullGroup R"           $ sendMessage $ pullGroup R)
  , ("M-C-k", addName "pullGroup U"           $ sendMessage $ pullGroup U)
  , ("M-C-j", addName "pullGroup D"           $ sendMessage $ pullGroup D)

  , ("M-C-m", addName "MergeAll"              $ withFocused (sendMessage . MergeAll))
  , ("M-C-u", addName "UnMerge"               $ withFocused (sendMessage . UnMerge))
  , ("M-C-/", addName "UnMergeAll"            $  withFocused (sendMessage . UnMergeAll))
  , ("M-C-.", addName "Switch focus next tab" $  onGroup W.focusUp')    -- Switch focus to next tab
  , ("M-C-,", addName "Switch focus prev tab" $  onGroup W.focusDown')  -- Switch focus to prev tab

  -- Custom bindings
  , ("M-<Esc>", spawn' "sysact")
  , ("M-<F11>", spawn' "brightness down")
  , ("M-<F12>", spawn' "brightness up")

  -- Print
  , ("<Print> s", spawn' "maimpick 'Selected'")
  , ("<Print> c", spawn' "maimpick 'Current'")
  , ("<Print> f", spawn' "maimpick 'Fullscreen'")
  , ("<Print> S-s", spawn' "maimpick 'Selected (copy)'")
  , ("<Print> S-c", spawn' "maimpick 'Current (copy)'")
  , ("<Print> S-f", spawn' "maimpick 'Fullscreen (copy)'")

  -- XF86
  , ("XF86MonBrightnessDown", spawn' "brightness down")
  , ("XF86MonBrightnessUp", spawn' "brightness up")
  , ("XF86AudioRaiseVolume", spawn' "volume up")
  , ("XF86AudioLowerVolume", spawn' "volume down")
  , ("XF86AudioMute", spawn' "volume mute")
  , ("XF86AudioMicMute", spawn' "mic-toggle")
  , ("XF86AudioPrev", spawn' "playerctl previous")
  , ("XF86AudioNext", spawn' "playerctl next")
  , ("XF86AudioPlay", spawn' "playerctl play-pause")

  -- MPC
  , ("M-[", spawn' "mpc prev")
  , ("M-]", spawn' "mpc next")
  , ("M-0", spawn' "mpc seek 0%")
  , ("M-=", spawn' "mpc volume +2 && mpc-volume")
  , ("M--", spawn' "mpc volume -2 && mpc-volume")
  , ("M-p", spawn' "mpc toggle")

  -- Leader keys
  , ("M-<Space> <Space>",  spawn' "dm-j4dmenu-desktop")
  , ("M-<Space> b",  spawn' "dm-win")
  , ("M-<Space> c",  spawn' "chromium")
  , ("M-<Space> d",  spawn' "dmenu_run -bw 0")
  , ("M-<Space> m",  spawn' "microphone toggle")
  , ("M-<Space> n", addName "Toggle ncmpcpp" $ namedScratchpadAction myScratchPads "ncmpcpp")
  , ("M-<Space> r",  spawn' (myTerminal ++ " -e lf"))
  , ("M-<Space> t", addName "Spawn terminal" $ spawn myTerminal)
  , ("M-<Space> v",  spawn' "neovide")
  , ("M-<Space> w",  spawn' "firefox")

  -- Leader opener keys
  , ("M-<Space> o d",  spawn' "which prime-run && prime-run discord || discord")
  , ("M-<Space> o a",  spawn' "atlauncher")
  , ("M-<Space> o l",  spawn' "logseq")
  , ("M-<Space> o t",  spawn' "zotero")
  , ("M-<Space> o z",  spawn' "zoom")

  -- Leader prompt keys
  , ("M-<Space> p a", spawn' "dm-man")
  , ("M-<Space> p c", spawn' "clipmenu")
  , ("M-<Space> p d", spawn' "dm-directory")
  , ("M-<Space> p C", spawn' "dm-colorscheme")
  , ("M-<Space> p e", spawn' "dm-emoji")
  , ("M-<Space> p k", spawn' "dm-kill")
  , ("M-<Space> p o", spawn' "dm-mount")
  , ("M-<Space> p p", spawn' "dm-passmenu")
  , ("M-<Space> p b", spawn' "dm-beats")
  , ("M-<Space> p s", spawn' "dm-scripts")
  , ("M-<Space> p u", spawn' "dm-umount")
  , ("M-<Space> p w", spawn' "weatherforecast")

  -- Leader system keys
  , ("M-<Space> ; a", spawn' "setwallpaper a2n")
  , ("M-<Space> ; d", spawn' "setwallpaper dt")
  , ("M-<Space> ; e", spawn' "setwallpaper elyk")
  , ("M-<Space> ; w", spawn' "nsxiv -rqto $XDG_PICTURES_DIR/wallpapers/*")
      --END_KEYS
  ]
  ^++^
    [ ("M-1 " ++ otherModMasks ++ [key], windows $ action tag)
      | (tag, key)  <- zip myWorkspaces "12345"
      , (otherModMasks, action) <- [
      ("", onlyOnScreen 0) ,
      ("S-", W.shift) ,
      ("M1-", W.greedyView) ,
      ("C-", swapWithCurrent)]
    ]
    ^++^
    [ ("M-2 " ++ otherModMasks ++ [key], windows $ action tag)
      | (tag, key)  <- zip myWorkspaces "12345"
      , (otherModMasks, action) <- [
      ("", onlyOnScreen 1) ,
      ("C-", swapWithCurrent)]
    ]
 where
  touchpadToggle =
    "(synclient | grep 'TouchpadOff.*1' && synclient TouchpadOff=0) || synclient TouchpadOff=1"
  toggleFloat w = windows
    (\s -> if M.member w (W.floating s)
      then W.sink w s
      else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
    )

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) --Set the window to floating mode and move by dragging
  , ((modMask, button2), \w -> focus w >> windows W.shiftMaster) --Raise the window to the top of the stack
  , ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w) --Set the window to floating mode and resize by dragging
  , ((modMask, button4), const $ moveTo Prev workspaceOnCurrentScreen) --Switch to previous workspace
  , ((modMask, button5), const $ moveTo Next workspaceOnCurrentScreen) --Switch to next workspace
  , ((modMask .|. shiftMask, button4), const $ shiftTo Prev workspaceOnCurrentScreen) --Send client to previous workspace
  , ((modMask .|. shiftMask, button5), const $ shiftTo Next workspaceOnCurrentScreen) --Send client to next workspace
  ]
-- }}}

------------------------------------------
---               Main                 ---
------------------------------------------

main :: IO ()
main =
  do
      xmonad
    . addEwmhWorkspaceSort (pure myFilter)
    . setEwmhActivateHook doAskUrgent
    . addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys 
    . ewmh
    . docks
    . rescreenHook rescreenCfg
    . dynamicEasySBs myStatusBarSpawner
    $ def
        { manageHook = insertPosition Below Newer
                       <+> myManageHook
                       <+> namedScratchpadManageHook myScratchPads
                       <+> manageDocks
        , focusFollowsMouse = True
        , clickJustFocuses = False
        , modMask = myModMask
        , terminal = myTerminal
        , startupHook = myStartupHook
        , mouseBindings = myMouseBindings
        , handleEventHook = myHandleEventHook
        , layoutHook = showWName' myShowWNameTheme myLayoutHook
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalColor
        , focusedBorderColor = myFocusColor
        , logHook = logHook def
                    <+> masterHistoryHook
                    <+> updatePointer (0.5, 0.5) (0.9, 0.9)
                    >> nsHideOnFocusLoss myScratchPads
        }

