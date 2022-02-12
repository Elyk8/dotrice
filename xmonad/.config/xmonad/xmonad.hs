------------------------------------------
---              Imports               ---
------------------------------------------

{-# LANGUAGE CPP #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PatternGuards #-}
-- {-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- {-# LANGUAGE TupleSections #-}

import Colors.Colors
import Control.Monad
import Data.Bits (testBit)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Semigroup
import Data.Tree
import Foreign.C (CInt)
import System.Directory
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import System.Posix.Files
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.SwapPromote
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers
  ( Side (CE, NW),
    composeOne,
    doCenterFloat,
    doFullFloat,
    doSideFloat,
    isDialog,
    isFullscreen,
    isInProperty,
    transience',
    (-?>),
  )
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar
  ( StatusBarConfig,
    dynamicEasySBs,
    dynamicSBs,
    statusBarProp,
    statusBarPropTo,
    withSB,
  )
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (..),
    NamedScratchpads,
    customFloating,
    namedScratchpadAction,
    namedScratchpadFilterOutWorkspacePP,
    namedScratchpadManageHook,
    nsHideOnFocusLoss,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

myFont :: String
myFont = "xft:Ubuntu:regular:size=9:antialias=true:hinting=true"

myBoldFont = "xft:monospace:weight=bold:size=50"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myTerminalClass :: String
myTerminalClass = "Alacritty"

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalColor :: String
myNormalColor = base08

myFocusColor :: String
myFocusColor = base05

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

mySpacing ::
  Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' ::
  Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myGaps :: Integer
myGaps = 12

------------------------------------------
---               Startup              ---
------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "~/.config/xmonad/scripts/trayer-launch.sh"

------------------------------------------
---           Multi Monitors           ---
------------------------------------------

-- multiScreenFocusHook :: Event -> X All
-- multiScreenFocusHook MotionEvent {ev_x = x, ev_y = y} = do
--   ms <- getScreenForPos x y
--   case ms of
--     Just cursorScreen -> do
--       let cursorScreenID = W.screen cursorScreen
--       focussedScreenID <- gets (W.screen . W.current . windowset)
--       when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
--       return (All True)
--     _ -> return (All True)
--   where
--     getScreenForPos ::
--       CInt ->
--       CInt ->
--       X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
--     getScreenForPos x y = do
--       ws <- windowset <$> get
--       let screens = W.current ws : W.visible ws
--           inRects = map (inRect x y . screenRect . W.screenDetail) screens
--       return $ fst <$> find snd (zip screens inRects)
--     inRect :: CInt -> CInt -> Rectangle -> Bool
--     inRect x y rect =
--       let l = fromIntegral (rect_x rect)
--           r = l + fromIntegral (rect_width rect)
--           t = fromIntegral (rect_y rect)
--           b = t + fromIntegral (rect_height rect)
--        in x >= l && x < r && y >= t && y < b
--     focusWS :: WorkspaceId -> X ()
--     focusWS ids = windows (W.view ids)
-- multiScreenFocusHook _ = return (All True)

rescreenCfg =
  def
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "setwallpaper"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

------------------------------------------
---               Theme                ---
------------------------------------------

myTabTheme =
  def
    { activeColor = base08,
      inactiveColor = basebg,
      urgentColor = base12,
      activeBorderColor = base08,
      inactiveBorderColor = basebg,
      urgentBorderColor = base04,
      activeTextColor = base10,
      inactiveTextColor = base13,
      urgentTextColor = base04,
      fontName = myFont,
      decoHeight = 30
    }

myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = myBoldFont,
      swn_fade = 0.6,
      swn_bgcolor = basebg,
      swn_color = base05
    }

toggleFloat x w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else
            if x == R
              then W.float w (W.RationalRect 0.5 0.015 0.5 1.0) s
              else W.float w (W.RationalRect 0.0 0.015 0.5 1.0) s
    )

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

------------------------------------------
---               Xmobar               ---
------------------------------------------

myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
  pure $
    statusBarPropTo
      ("_XMONAD_LOG_" ++ show s)
      ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobar" ++ show s ++ ".hs")
      (pure $ myXmobarPP (S s))

myXmobarPP :: ScreenId -> PP
myXmobarPP s =
  filterOutWsPP [scratchpadWorkspaceTag] $
    def
      { ppSep = " ",
        ppWsSep = "",
        ppCurrent = xmobarColor base04 "" . clickable wsIconFull,
        ppVisible = xmobarColor base04 "" . clickable wsIconHidden,
        ppVisibleNoWindows = Just (xmobarColor base04 "" . clickable wsIconEmpty),
        ppHidden = xmobarColor basefg "" . clickable wsIconHidden,
        ppHiddenNoWindows = xmobarColor basefg "" . clickable wsIconEmpty,
        ppUrgent = xmobarColor base05 "" . (wrap "!" "!" . clickable wsIconFull),
        ppOrder = \(ws : _ : _ : extras) -> ws : extras,
        ppExtras =
          [ windowCount,
            wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix $
              wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix $
                wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix $
                  wrapL "<fc=#666666><fn=1>|</fn> </fc>" "<fc=#666666> <fn=1>|</fn></fc>" $
                    layoutColorIsActive s (logLayoutOnScreen s),
            wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix $
              titleColorIsActive s (shortenL 50 $ logTitleOnScreen s)
          ]
      }
  where
    wsIconFull = "\61713 "
    wsIconHidden = "\61842 "
    wsIconEmpty = "\61708 "
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l
    layoutColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l

------------------------------------------
---             WorkSpaces             ---
------------------------------------------

myWorkspaces :: [[Char]]
myWorkspaces = ["1", "2", "3", "4", "5"]

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x : xs) ws =
  addActions
    xs
    (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
  where
    k = fst x
    b = snd x

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws =
  addActions
    [(show i, 1), ("q", 2), ("Left", 4), ("Right", 5)]
    icon
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

myFilter = filterOutWs [scratchpadWorkspaceTag]

------------------------------------------
---              Layouts               ---
------------------------------------------

myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        windowNavigation $
          T.toggleLayouts floats $
            mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
      withBorder myBorderWidth tall
        ||| wide
        ||| noBorders monocle
        -- ||| noBorders tabs
        -- ||| grid
        -- ||| spirals
        -- ||| three
        -- ||| accordion
        -- ||| floats

tall =
  renamed [Replace "tall"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            Mag.magnifierOff $
              mkToggle (single MIRROR) $
                limitWindows 5 $
                  mySpacing myGaps $
                    ResizableTall 1 (3 / 100) (1 / 2) []

wide =
  renamed [Replace "wide"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            Mag.magnifierOff $
              Mirror $
                limitWindows 5 $
                  mySpacing myGaps $
                    ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "monocle"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 20 Full

-- grid =
--   renamed [Replace "grid"] $
--     avoidStruts $
--       smartBorders $
--         addTabs shrinkText myTabTheme $
--           subLayout [] (smartBorders Simplest) $
--             limitWindows 12 $
--               mySpacing myGaps $
--                 Mag.magnifierOff $
--                   mkToggle (single MIRROR) $
--                     Grid (16 / 10)

-- spirals =
--   renamed [Replace "fibonacci"] $
--     avoidStruts $
--       smartBorders $
--         addTabs shrinkText myTabTheme $
--           subLayout [] (smartBorders Simplest) $
--             Mag.magnifierOff $
--               mkToggle (single MIRROR) $
--                 mySpacing' myGaps $
--                   spiral (6 / 7)

-- three =
--   renamed [Replace "three"] $
--     avoidStruts $
--       smartBorders $
--         addTabs shrinkText myTabTheme $
--           subLayout [] (smartBorders Simplest) $
--             limitWindows 7 $
--               Mag.magnifierOff $
--                 mkToggle (single MIRROR) $
--                   ThreeCol 1 (3 / 100) (1 / 2)

floats =
  renamed [Replace "floats"] $
    avoidStruts $
      Mag.magnifierOff $
        smartBorders $
          limitWindows
            20
            simplestFloat

-- tabs = renamed [Replace "tabs"] $ tabbed shrinkText myTabTheme

-- accordion = renamed [Replace "accordion"] $ mkToggle (single MIRROR) Accordion

------------------------------------------
---            Window Rules            ---
------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [title =? t --> doIgnore | t <- myIgnores],
        [className =? c --> doShift (head myWorkspaces) | c <- myW1C],
        [className =? c --> doShift (myWorkspaces !! 2) | c <- myW3C],
        [className =? c --> doShift (myWorkspaces !! 3) | c <- myW4C],
        [className =? c --> doShift (myWorkspaces !! 4) | c <- myW5C],
        [className =? c --> doFloat | c <- myFloatC],
        [className =? c --> doCenterFloat | c <- myFloatCC],
        [name =? n --> doSideFloat NW | n <- myFloatSN],
        [name =? n --> doF W.focusDown | n <- myFocusDC],
        [role =? "LINE" --> doFloat],
        [role =? "GtkFileChooserDialog" --> doFloat],
        [role =? "pop-up" --> doCenterFloat],
        [iconName =? "Gvim        " --> doFloat],
        [iconName =? "Launch Application" --> doFloat],
        [isFullscreen --> doFullFloat]
      ]
  where
    name = stringProperty "WM_NAME"
    role = stringProperty "WM_WINDOW_ROLE"
    iconName = stringProperty "WM_ICON_NAME"
    myIgnores = ["SafeEyes-0", "SafeEyes-1"]
    myW1C = ["Minecraft Launcher", "ftb-app"]
    myW3C = ["Brave-browser"]
    myW4C = ["zoom", "discord"]
    myW5C = ["VirtualBox Manager", "VirtualBox Machine", "Thunderbird"]
    myFloatC =
      [ "confirm",
        "file_progress",
        "dialog",
        "download",
        "error",
        "toolbar",
        "Gmrun"
      ]
    myFloatCC =
      [ "notification",
        "Yad",
        "Xfce4-power-manager-settings",
        "Dragon-drag-and-drop"
      ]
    myFloatSN = ["Event Tester"]
    myFocusDC = ["Event Tester", "Notify-osd"]

myHandleEventHook :: Event -> X All
myHandleEventHook =
  swallowEventHook (className =? myTerminalClass) (return True)
    <+> Hacks.trayerAboveXmobarEventHook
    <+> Hacks.windowedFullscreenFixEventHook

------------------------------------------
---            ScratchPads             ---
------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS
      "ncmpcpp"
      launchNcmpcpp
      (appName =? "ncmpcpp")
      (customFloating $ W.RationalRect l t w h),
    NS
      "terminal"
      launchTerminal
      (appName =? "scratchpad")
      (customFloating $ W.RationalRect l t w h)
  ]
  where
    launchNcmpcpp = myTerminal ++ " --class ncmpcpp -e ncmpcpp"
    launchTerminal = myTerminal ++ " --class scratchpad"
    h = 0.8
    w = 0.6
    t = (1 - h) / 2
    l = (1 - w) / 2

------------------------------------------
---                Keys                ---
------------------------------------------
--START_KEYS
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [ -- Xmonad
    ("M-C-r", spawn "xmonad --restart; killall xmobar"),
    ("M-S-<Esc>", io exitSuccess),
    -- Kill windows
    ("M-q", kill1), -- Kill the currently focused client
    ("M-S-q", killAll), -- Kill all windows on current workspace

    -- Sticky Windows
    ("M-v", windows copyToAll), -- Make focused window always visible in all workspaces
    ("M-S-v", killAllOtherCopies), -- Toggle window state back

    -- Floating windows
    ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
    ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-C-s", sinkAll), -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    ("C-M1-j", decWindowSpacing 4), -- Decrease window spacing
    ("C-M1-k", incWindowSpacing 4), -- Increase window spacing
    ("C-M1-h", decScreenSpacing 4), -- Decrease screen spacing
    ("C-M1-l", incScreenSpacing 4), -- Increase screen spacing
    ("C-M1-m", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled), -- Toggle gaps

    -- Dual monitor switcher
    ("M-w", onNextNeighbour def W.greedyView),
    ("M-S-w", onNextNeighbour def W.shift),
    ("M-s", onNextNeighbour def W.view),
    -- Windows navigation
    ("M-b", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
    ("M-S-b", windows W.swapMaster), -- Swap the focused window and the master window
    ("M-S-j", windows W.swapDown), -- Swap focused window with next window
    ("M-S-k", windows W.swapUp), -- Swap focused window with prev window
    ("M-<Backspace>", whenX (swapHybrid True) dwmpromote), -- Swap master window and last swapped window or first window in stack
    ("M-S-<Tab>", rotSlavesDown), -- Rotate all windows except master and keep focus in place
    ("M-C-<Tab>", rotAllDown), -- Rotate all the windows in the current stack

    -- Layouts
    ("M-<Space>", sendMessage NextLayout), -- Switch to next layout
    ("M-<Tab>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full
    ("M-r", sendMessage Mag.Toggle), -- Zoom focused client

    -- Increase/decrease windows in the master pane or the stack
    ("M-S-<Up>", sendMessage (IncMasterN 1)), -- Increase # of clients master pane
    ("M-S-<Down>", sendMessage (IncMasterN (-1))), -- Decrease # of clients master pane
    ("M-C-<Up>", increaseLimit), -- Increase # of windows
    ("M-C-<Down>", decreaseLimit), -- Decrease # of windows

    -- Window resizing
    ("M-h", sendMessage Shrink), -- Shrink horiz window width
    ("M-l", sendMessage Expand), -- Expand horiz window width
    ("M-M1-j", sendMessage MirrorShrink), -- Shrink vert window width
    ("M-M1-k", sendMessage MirrorExpand), -- Expand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
    ("M-C-h", sendMessage $ pullGroup L),
    ("M-C-l", sendMessage $ pullGroup R),
    ("M-C-k", sendMessage $ pullGroup U),
    ("M-C-j", sendMessage $ pullGroup D),
    ("M-C-m", withFocused (sendMessage . MergeAll)),
    -- , ("M-C-u", withFocused (sendMessage . UnMerge))
    ("M-C-/", withFocused (sendMessage . UnMergeAll)),
    ("M-C-.", onGroup W.focusUp'),
    -- Switch focus to next tab
    ("M-C-,", onGroup W.focusDown'), -- Switch focus to prev tab

    -- Scratchpad windows
    ("M-n n", namedScratchpadAction myScratchPads "ncmpcpp"), -- Ncmpcpp Player
    ("M-n t", namedScratchpadAction myScratchPads "terminal") -- Terminal
  ]
    --END_KEYS
    ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
         | (tag, key) <- zip myWorkspaces "12345",
           (otherModMasks, action) <-
             [ ("", windows . W.view), -- was W.greedyView
               ("S-", windows . W.shift),
               ("C-", windows . copy)
             ]
       ]

myRemovedKeys :: [String]
myRemovedKeys = ["M-e", "M-S-e"]

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ( (modMask, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster --Set the window to floating mode and move by dragging
      ),
      ((modMask, button2), \w -> focus w >> windows W.shiftMaster), --Raise the window to the top of the stack
      ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w), --Set the window to floating mode and resize by dragging
      ((modMask, button4), const $ moveTo Prev workspaceOnCurrentScreen), --Switch to previous workspace
      ((modMask, button5), const $ moveTo Next workspaceOnCurrentScreen), --Switch to next workspace
      ( (modMask .|. shiftMask, button4),
        const $ shiftTo Prev workspaceOnCurrentScreen --Send client to previous workspace
      ),
      ( (modMask .|. shiftMask, button5),
        const $ shiftTo Next workspaceOnCurrentScreen --Send client to next workspace
      )
    ]

------------------------------------------
---               Main                 ---
------------------------------------------

main :: IO ()
main =
  do
    xmonad
    . addEwmhWorkspaceSort (pure myFilter)
    . setEwmhActivateHook doAskUrgent
    . ewmh
    . docks
    . rescreenHook rescreenCfg
    . dynamicSBs myStatusBarSpawner
    $ def
      { manageHook =
          insertPosition Master Newer
            <+> myManageHook
            <+> namedScratchpadManageHook myScratchPads
            <+> manageDocks,
        focusFollowsMouse = True,
        clickJustFocuses = False,
        modMask = myModMask,
        terminal = myTerminal,
        startupHook = myStartupHook,
        mouseBindings = myMouseBindings,
        handleEventHook = myHandleEventHook,
        layoutHook = showWName' myShowWNameTheme myLayoutHook,
        workspaces = myWorkspaces,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalColor,
        focusedBorderColor = myFocusColor,
        logHook =
          logHook def
            <+> updatePointer (0.5, 0.5) (0, 0)
            <+> masterHistoryHook
            <+> refocusLastLogHook
            >> nsHideOnFocusLoss myScratchPads
      }
      `additionalKeysP` myAdditionalKeys
      `removeKeysP` myRemovedKeys
