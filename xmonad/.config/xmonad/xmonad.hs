------------------------------------------
---              Imports               ---
------------------------------------------
{-# LANGUAGE LambdaCase #-}

-- set turn tile

--(workspaceHistoryHook)

import qualified Codec.Binary.UTF8.String as UTF8
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
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.SwapPromote
import XMonad.Actions.SwapWorkspaces
-- import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (Side (CE, NW), composeOne, doCenterFloat, doFullFloat, doSideFloat, isDialog, isFullscreen, isInProperty, transience', (-?>))
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicEasySBs, dynamicSBs, statusBarProp, statusBarPropTo, withSB)
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.Magnifier
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
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties

myFont :: String
myFont = "xft:Ubuntu Nerd Font:regular:size=9:antialias=true:hinting=true"

myBoldFont = "xft:Mononoki Nerd Font:weight=bold:size=50"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myTerminalClass :: String
myTerminalClass = "St"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalColor :: String
myNormalColor = basebg

myFocusColor :: String
myFocusColor = base05

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

------------------------------------------
---               Startup              ---
------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "killall trayer"
  spawn ("sleep 1 && trayer -l --edge top --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint " ++ colorTrayer ++ " --monitor 1 --height 30 --iconspacing 4")
  spawnOnce "~/.config/xmonad/scripts/autostart.sh"

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

------------------------------------------
---           Pointer Update           ---
------------------------------------------

-- newtype MyUpdatePointerActive = MyUpdatePointerActive Bool

-- instance ExtensionClass MyUpdatePointerActive where
--   initialValue = MyUpdatePointerActive True

-- myUpdatePointer :: (Rational, Rational) -> (Rational, Rational) -> X ()
-- myUpdatePointer refPos ratio =
--   whenX isActive $ do
--     dpy <- asks display
--     root <- asks theRoot
--     (_, _, _, _, _, _, _, m) <- io $ queryPointer dpy root
--     unless (testBit m 9 || testBit m 8 || testBit m 10) $ -- unless the mouse is clicking
--       updatePointer refPos ratio
--   where
--     isActive = (\(MyUpdatePointerActive b) -> b) <$> XS.get

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

-- workspaceOnCurrentScreen :: WSType
-- workspaceOnCurrentScreen = WSIs $ do
--   s <- currentScreen
--   return $ \x -> W.tag x /= "NSP" && isOnScreen s x

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
      ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobarrc" ++ show s)
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
          [ logCurrentOnScreen s,
            wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix $
              wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix $
                wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix $
                  wrapL "<fc=#666666><fn=1>|</fn> </fc>" "<fc=#666666> <fn=1>|</fn></fc>" $ layoutColorIsActive s (logLayoutOnScreen s),
            wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix $
              titleColorIsActive s (shortenL 60 $ logTitleOnScreen s) .| logWhenActive 0 (logConst "*") .| logConst "There's nothing here"
          ]
      }
  where
    wsIconFull = "\61713 "
    wsIconHidden = "\61842 "
    wsIconEmpty = "\61708 "
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c
        then xmobarColorL base03 "" l
        else xmobarColorL base05 "" l
    layoutColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c
        then xmobarColorL base03 "" l
        else xmobarColorL base05 "" l

------------------------------------------
---             WorkSpaces             ---
------------------------------------------

myWorkspaces :: [[Char]]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x : xs) ws = addActions xs (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
  where
    k = fst x
    b = snd x

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws = addActions [(show i, 1), ("q", 2), ("Left", 4), ("Right", 5)] icon
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

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
        ||| zoom
        ||| noBorders monocle
        ||| noBorders tabs
        ||| grid
        ||| spirals
        ||| threeCol
        ||| threeRow
        ||| tallAccordion
        ||| wideAccordion
        ||| floats

tall =
  renamed [Replace "tall"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 12 $
              mySpacing 8 $
                ResizableTall 1 (3 / 100) (1 / 2) []

zoom =
  renamed [Replace "zoom"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            magnifier $
              limitWindows 12 $
                mySpacing 8 $
                  ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "monocle"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 20 Full

grid =
  renamed [Replace "grid"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 12 $
              mySpacing 8 $
                mkToggle (single MIRROR) $
                  Grid (16 / 10)

spirals =
  renamed [Replace "spirals"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            mySpacing' 8 $
              spiral (6 / 7)

threeCol =
  renamed [Replace "threeCol"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 7 $
              ThreeCol 1 (3 / 100) (1 / 2)

threeRow =
  renamed [Replace "threeRow"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 7 $
              Mirror $
                ThreeCol 1 (3 / 100) (1 / 2)

floats =
  renamed [Replace "floats"] $
    avoidStruts $
      smartBorders $
        limitWindows 20 simplestFloat

tabs =
  renamed [Replace "tabs"] $
    tabbed shrinkText myTabTheme

tallAccordion =
  renamed
    [Replace "tallAccordion"]
    Accordion

wideAccordion =
  renamed [Replace "wideAccordion"] $
    Mirror Accordion

------------------------------------------
---            Window Rules            ---
------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll . concat $
    [ [title =? t --> doIgnore | t <- myIgnores],
      [className =? c --> doShift (head myWorkspaces) | c <- myW1S],
      [className =? c --> doShift (myWorkspaces !! 3) | c <- myW4S],
      [className =? c --> doShift (myWorkspaces !! 4) | c <- myW5S],
      [className =? c --> doShift (myWorkspaces !! 5) | c <- myW6S],
      [className =? c --> doShift (myWorkspaces !! 6) | c <- myW7S],
      [className =? c --> doShift (myWorkspaces !! 7) | c <- myW8S],
      [className =? c --> doShift (myWorkspaces !! 8) | c <- myW9S],
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
    myW1S = ["obsidian"]
    myW4S = ["Brave-browser"]
    myW5S = ["VirtualBox Manager", "VirtualBox Machine"]
    myW6S = ["qBittorrent"]
    myW7S = ["zoom"]
    myW8S = ["discord"]
    myW9S = ["Thunderbird"]
    myFloatC = ["confirm", "file_progress", "dialog", "download", "error", "toolbar", "Gmrun"]
    myFloatCC =
      [ "notification",
        "Yad",
        "Xfce4-power-manager-settings",
        "Dragon-drag-and-drop",
        "scratchpad",
        "ncmpcpp"
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
  [ NS "ncmpcpp" launchNcmpcpp (className =? "ncmpcpp") (customFloating $ W.RationalRect h w t l),
    NS "terminal" launchTerminal (className =? "scratchpad") (customFloating $ W.RationalRect h w t l)
  ]
  where
    launchNcmpcpp = myTerminal ++ " -c ncmpcpp -e ncmpcpp"
    launchTerminal = myTerminal ++ " -c scratchpad"
    h = 0.8
    w = 0.8
    t = 0.95 - h
    l = 0.95 - w

------------------------------------------
---                Keys                ---
------------------------------------------
-- START_KEYS
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [ -- Xmonad
    ("M-C-r", spawn "xmonad --restart; killall xmobar"),
    ("M-S-<Esc>", io exitSuccess),
    -- Kill windows
    ("M-q", kill), -- Kill the currently focused client
    ("M-S-q", killAll), -- Kill all windows on current workspace

    -- Dual monitor switcher
    ("M-w", onNextNeighbour def W.view),
    ("M-S-w", onNextNeighbour def W.shift),
    ("M-r", onNextNeighbour def W.greedyView),
    ("M-S-r", onNextNeighbour def W.shift),
    -- Floating windows
    ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
    ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-C-s", sinkAll), -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    ("C-M1-j", decWindowSpacing 4), -- Decrease window spacing
    ("C-M1-k", incWindowSpacing 4), -- Increase window spacing
    ("C-M1-h", decScreenSpacing 4), -- Decrease screen spacing
    ("C-M1-l", incScreenSpacing 4), -- Increase screen spacing

    -- Window Warp
    ("M-b", warpToWindow (1%2) (1%2)), -- Warp to focused window

    -- Windows navigation
    ("M-n", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
    ("M-S-n", windows W.swapMaster), -- Swap the focused window and the master window
    ("M-S-j", windows W.swapDown), -- Swap focused window with next window
    ("M-S-k", windows W.swapUp), -- Swap focused window with prev window
    ("M-<Backspace>", whenX (swapHybrid True) dwmpromote), -- Swap master window and last swapped window or first window in stack
    ("M-S-<Tab>", rotSlavesDown), -- Rotate all windows except master and keep focus in place
    ("M-C-<Tab>", rotAllDown), -- Rotate all the windows in the current stack

    -- Layouts
    ("M-<Space>", sendMessage NextLayout), -- Switch to next layout
    ("M-<Tab>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full

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
    ("M-C-.", onGroup W.focusUp'), -- Switch focus to next tab
    ("M-C-,", onGroup W.focusDown'), -- Switch focus to prev tab

    -- Scratchpad windows
    ("M-s n", namedScratchpadAction myScratchPads "ncmpcpp"), -- Ncmpcpp Player
    ("M-s t", namedScratchpadAction myScratchPads "terminal") -- Terminal
  ]

-- END_KEYS

-- Workspaces
-- ("M-.", nextScreen), -- Switch focus to next monitor
-- ("M-,", prevScreen), -- Switch focus to prev monitor
-- ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP), -- Shifts focused window to next ws
-- ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP), -- Shifts focused window to prev ws

myKeys conf =
  let modm = modMask conf
   in M.fromList $
        ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) :
          [ ((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
              (f, m) <-
                [ (W.greedyView, controlMask),
                  (W.view, 0),
                  (liftM2 (.) W.view W.shift, shiftMask),
                  (swapWithCurrent, mod1Mask)
                ]
          ]

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster), --Set the window to floating mode and move by dragging
      ((modMask, button2), \w -> focus w >> windows W.shiftMaster), --Raise the window to the top of the stack
      ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w), --Set the window to floating mode and resize by dragging
      ((modMask, button4), const $ moveTo Prev nonNSP), --Switch to previous workspace
      ((modMask, button5), const $ moveTo Next nonNSP), --Switch to next workspace
      ((modMask .|. shiftMask, button4), const $ shiftTo Prev nonNSP), --Send client to previous workspace
      ((modMask .|. shiftMask, button5), const $ shiftTo Next nonNSP) --Send client to next workspace
    ]
    where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

------------------------------------------
---               Main                 ---
------------------------------------------

main :: IO ()
main = do
  xmonad . ewmh . docks . dynamicSBs myStatusBarSpawner $
    def
      { manageHook = myManageHook <+> manageDocks,
        focusFollowsMouse = True,
        clickJustFocuses = False,
        modMask = myModMask,
        terminal = myTerminal,
        startupHook = myStartupHook,
        keys = myKeys,
        mouseBindings = myMouseBindings,
        handleEventHook = myHandleEventHook,
        layoutHook = showWName' myShowWNameTheme $ myLayoutHook,
        workspaces = myWorkspaces,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalColor,
        focusedBorderColor = myFocusColor,
        logHook =
          logHook def
            -- <+> myUpdatePointer (0.5, 0.5) (0, 0)
            <+> masterHistoryHook
            <+> refocusLastLogHook
            >> nsHideOnFocusLoss myScratchPads
      }
      `additionalKeysP` myAdditionalKeys
