-- {{{ IMPORTS
import           Colors.Colors
import           Control.Monad
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.Tree
import           Foreign.C                      ( CInt )
import           System.Directory
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hPutStrLn )
import           System.Posix.Files
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.MouseResize
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.RotSlaves       ( rotAllDown
                                                , rotSlavesDown
                                                )
import           XMonad.Actions.Submap
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.UpdatePointer   ( updatePointer )
import           XMonad.Actions.WindowGo        ( runOrRaise )
import           XMonad.Actions.WithAll         ( killAll
                                                , sinkAll
                                                )
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
import           XMonad.Hooks.RefocusLast       ( refocusLastLogHook )
import           XMonad.Hooks.Rescreen
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
                                                ( Toggle(..) )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers(MIRROR, NBFULL, NOBORDERS) )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( ToggleLayout(Toggle)
                                                , toggleLayouts
                                                )
import           XMonad.Layout.WindowArranger   ( WindowArrangerMsg(..)
                                                , windowArrange
                                                )
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState   as XS
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
myFont = "xft:Ubuntu:regular:size=9:antialias=true:hinting=true"

myBoldFont = "xft:monospace:weight=bold:size=50"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myTerminalClass :: String
myTerminalClass = "Alacritty"

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalColor :: String
myNormalColor = base08

myFocusColor :: String
myFocusColor = base05

myLeader :: String
myLeader = "M-<Space> "

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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
multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent { ev_x = x, ev_y = y } = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focussedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
      return (All True)
    _ -> return (All True)
 where
  getScreenForPos :: CInt -> CInt -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
  getScreenForPos x y = do
    ws <- windowset <$> get
    let screens = W.current ws : W.visible ws
        inRects = map (inRect x y . screenRect . W.screenDetail) screens
    return $ fst <$> find snd (zip screens inRects)
  inRect :: CInt -> CInt -> Rectangle -> Bool
  inRect x y rect =
    let l = fromIntegral (rect_x rect)
        r = l + fromIntegral (rect_width rect)
        t = fromIntegral (rect_y rect)
        b = t + fromIntegral (rect_height rect)
    in  x >= l && x < r && y >= t && y < b
  focusWS :: WorkspaceId -> X ()
  focusWS ids = windows (W.view ids)
multiScreenFocusHook _ = return (All True)

rescreenCfg = def { afterRescreenHook = myAfterRescreenHook, randrChangeHook = myRandrChangeHook }

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "setwallpaper"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

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
myTabTheme = def { activeColor         = base08
                 , inactiveColor       = basebg
                 , urgentColor         = base12
                 , activeBorderColor   = base08
                 , inactiveBorderColor = basebg
                 , urgentBorderColor   = base04
                 , activeTextColor     = base10
                 , inactiveTextColor   = base13
                 , urgentTextColor     = base04
                 , fontName            = myFont
                 , decoHeight          = 30
                 }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def { swn_font = myBoldFont, swn_fade = 0.6, swn_bgcolor = basebg, swn_color = base05 }

toggleFloat x w = windows
  (\s -> if M.member w (W.floating s)
    then W.sink w s
    else if x == R then W.float w (W.RationalRect 0.5 0.015 0.5 1.0) s else W.float w (W.RationalRect 0.0 0.015 0.5 1.0) s
  )
-- }}}

-- {{{ XMOBAR
myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
  pure
    $ statusBarPropTo ("_XMONAD_LOG_" ++ show s) ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobar" ++ show s ++ ".hs") (pure $ myXmobarPP (S s))

myXmobarPP :: ScreenId -> PP
myXmobarPP s = filterOutWsPP [scratchpadWorkspaceTag] $ def
  { ppSep              = "<fc=" ++ base08 ++ "> <fn=1>|</fn> </fc>"
  , ppCurrent          = xmobarColor base03 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ base03 ++ ">") "</box>"
  , ppVisible          = xmobarColor basefg "" . wrap ("<box type=Top width=2 mt=2 color=" ++ basefg ++ ">") "</box>" . clickable
  , ppVisibleNoWindows = Just (xmobarColor base04 "" . clickable)
  , ppHidden           = xmobarColor base04 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ base04 ++ ">") "</box>" . clickable
  , ppHiddenNoWindows  = xmobarColor basefg "" . clickable
  , ppUrgent           = xmobarColor base05 "" . wrap "!" "!"
  , ppOrder            = \(ws : _ : _ : extras) -> ws : extras
  , ppExtras           = [windowCount, layoutColorIsActive s (logLayoutOnScreen s), titleColorIsActive s (shortenL 50 $ logTitleOnScreen s)]
  }
 where
  titleColorIsActive n l = do
    c <- withWindowSet $ return . W.screen . W.current
    if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l
  layoutColorIsActive n l = do
    c <- withWindowSet $ return . W.screen . W.current
    if n == c then xmobarColorL base03 "" l else xmobarColorL base05 "" l
-- }}}

-- {{{ WORKSPACES
myWorkspaces :: [[Char]]
myWorkspaces = ["dev", "sys", "www", "dis", "msg"]

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>" where i = fromJust $ M.lookup ws myWorkspaceIndices

myFilter = filterOutWs [scratchpadWorkspaceTag]
-- }}}

-- {{{ LAYOUTS
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ windowNavigation $ T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                                                                                                                myDefaultLayout
  where myDefaultLayout = withBorder myBorderWidth tall ||| wide ||| noBorders monocle
        -- ||| noBorders tabs
        -- ||| grid
        -- ||| spirals
        -- ||| three
        -- ||| accordion
        -- ||| floats

tall =
  renamed [Replace "tall"]
    $ avoidStruts
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
    $ avoidStruts
    $ smartBorders
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ Mag.magnifierOff
    $ Mirror
    $ limitWindows 5
    $ mySpacing myGaps
    $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "monocle"] $ avoidStruts $ smartBorders $ addTabs shrinkText myTabTheme $ subLayout [] (smartBorders Simplest) $ limitWindows
    20
    Full

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

floats = renamed [Replace "floats"] $ avoidStruts $ Mag.magnifierOff $ smartBorders $ limitWindows 20 simplestFloat

-- tabs = renamed [Replace "tabs"] $ tabbed shrinkText myTabTheme

-- accordion = renamed [Replace "accordion"] $ mkToggle (single MIRROR) Accordion
-- }}}

-- {{{ WINDOW RULES
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [ title =? t --> doIgnore | t <- myIgnores ]
      , [ className =? c --> doShift (head myWorkspaces) | c <- myW1C ]
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
      ]
 where
  name      = stringProperty "WM_NAME"
  role      = stringProperty "WM_WINDOW_ROLE"
  iconName  = stringProperty "WM_ICON_NAME"
  myIgnores = ["SafeEyes-0", "SafeEyes-1"]
  myW1C     = ["obsidian", "VSCodium"]
  myW3C     = ["Brave-browser"]
  myW4C     = ["zoom", "discord"]
  myW5C     = ["VirtualBox Manager", "VirtualBox Machine", "Thunderbird"]
  myFloatC  = ["confirm", "file_progress", "dialog", "download", "error", "toolbar", "Gmrun"]
  myFloatCC = ["notification", "Yad", "Xfce4-power-manager-settings", "Dragon-drag-and-drop"]
  myFloatSN = ["Event Tester"]
  myFocusDC = ["Event Tester", "Notify-osd"]

myHandleEventHook :: Event -> X All
myHandleEventHook =
  swallowEventHook (className =? myTerminalClass) (return True)
    <+> multiScreenFocusHook
    <+> Hacks.trayerAboveXmobarEventHook
    <+> Hacks.windowedFullscreenFixEventHook
-- }}}

-- {{{ SCRATCHPADS
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "ncmpcpp"  launchNcmpcpp  (appName =? "ncmpcpp")    (customFloating $ W.RationalRect l t w h)
  , NS "terminal" launchTerminal (appName =? "scratchpad") (customFloating $ W.RationalRect l t w h)
  ]
 where
  launchNcmpcpp  = myTerminal ++ " --class ncmpcpp -e ncmpcpp"
  launchTerminal = myTerminal ++ " --class scratchpad"
  h              = 0.8
  w              = 0.6
  t              = (1 - h) / 2
  l              = (1 - w) / 2
-- }}}

-- {{{ KEYBINDINGS
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [
--START_KEYS
    -- Xmonad
    ("M-C-r", spawn "xmonad --restart; killall xmobar")
    , ("M-S-<Esc>"              , io exitSuccess)

    -- Kill windows
    , ("M-q"                    , kill1) -- Kill the currently focused client
    , ("M-S-q"                  , killAll) -- Kill all windows on current workspace

    -- Useful programs to have a keybinding for launch
    , ("M-<Esc>"                , spawn "arcolinux-logout") -- Exit Prompt using Arcolinux Logout

    -- Sticky Windows
    , ("M-v"                    , windows copyToAll) -- Make focused window always visible in all workspaces
    , ("M-S-v"                  , killAllOtherCopies) -- Toggle window state back

    -- Floating windows
    , ("M-f"                    , sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
    , ("M-t"                    , withFocused $ windows . W.sink) -- Push floating window back to tile
    , ("M-C-s"                  , sinkAll) -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    , ("C-M1-j"                 , decWindowSpacing 4) -- Decrease window spacing
    , ("C-M1-k"                 , incWindowSpacing 4) -- Increase window spacing
    , ("C-M1-h"                 , decScreenSpacing 4) -- Decrease screen spacing
    , ("C-M1-l"                 , incScreenSpacing 4) -- Increase screen spacing
    , ("C-M1-m", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled) -- Toggle gaps

    -- Dual monitor switcher
    , ("M-w"                    , onNextNeighbour def W.greedyView)
    , ("M-e"                    , onNextNeighbour def W.view)
    , ("M-S-w"                  , onNextNeighbour def W.shift)

    -- Windows navigation
    , ("M-m"                    , windows W.focusMaster) -- Move focus to the master window
    , ("M-j"                    , windows W.focusDown) -- Move focus to the next window
    , ("M-k"                    , windows W.focusUp) -- Move focus to the prev window
    , ("M-S-m"                  , windows W.swapMaster) -- Swap the focused window and the master window
    , ("M-S-j"                  , windows W.swapDown) -- Swap focused window with next window
    , ("M-S-k"                  , windows W.swapUp) -- Swap focused window with prev window
    , ("M-<Backspace>"          , whenX (swapHybrid True) dwmpromote) -- Swap master window and last swapped window or first window in stack
    , ("M-S-<Tab>"              , rotSlavesDown) -- Rotate all windows except master and keep focus in place
    , ("M-C-<Tab>"              , rotAllDown) -- Rotate all the windows in the current stack

    -- Layouts
    , ("M-b"                    , sendMessage NextLayout) -- Switch to next layout
    , ("M-<Tab>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
    , ("M-r"                    , sendMessage Mag.Toggle) -- Zoom focused client

    -- Increase/decrease windows in the master pane or the stack
    , ("M-S-<Up>"               , sendMessage (IncMasterN 1)) -- Increase # of clients master pane
    , ("M-S-<Down>"             , sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
    , ("M-C-<Up>"               , increaseLimit) -- Increase # of windows
    , ("M-C-<Down>"             , decreaseLimit) -- Decrease # of windows

    -- Window resizing
    , ("M-h"                    , sendMessage Shrink) -- Shrink horiz window width
    , ("M-l"                    , sendMessage Expand) -- Expand horiz window width
    , ("M-M1-j"                 , sendMessage MirrorShrink) -- Shrink vert window width
    , ("M-M1-k"                 , sendMessage MirrorExpand) -- Expand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
    , ("M-C-h"                  , sendMessage $ pullGroup L)
    , ("M-C-l"                  , sendMessage $ pullGroup R)
    , ("M-C-k"                  , sendMessage $ pullGroup U)
    , ("M-C-j"                  , sendMessage $ pullGroup D)
    , ("M-C-m", withFocused (sendMessage . MergeAll))
    , ("M-C-u"                  , withFocused (sendMessage . UnMerge))
    , ("M-C-/", withFocused (sendMessage . UnMergeAll))
    , ("M-C-."                  , onGroup W.focusUp')

    -- Switch focus to next tab
    , ("M-C-,"                  , onGroup W.focusDown') -- Switch focus to prev tab

    -- Music control
    , ("M-0", spawn "mpc seek 0%") -- Restart song
    , ("M-,", spawn "mpc seek -10") -- Backward 10 secs
    , ("M-.", spawn "mpc seek +10") -- Forward 10 secs
    , ("M-[", spawn "mpc prev") -- Previous song
    , ("M-]", spawn "mpc next") -- Next song
    , ("M--", spawn "mpc volume -2") -- Volume down -2
    , ("M-=", spawn "mpc volume +2") -- Volume up +2
    , ("M-p", spawn "mpc toggle") -- Pause/play
    , ("M-r", spawn "mpc repeat") -- Toggle repeat mode
    , ("M-s", spawn "mpc pause ; pauseallmpv") -- Stop

    -- System
    , ("<XF86Calculator>"       , spawn (myTerminal ++ " -e bc -l"))
    , ("<XF86DOS>"              , spawn myTerminal)
    , ("<XF86Launch1>"          , spawn "xset dpms force off")
    , ("<XF86Mail>"             , spawn "/usr/bin/thunderbird")
    , ("<XF86MonBrightnessDown>", spawn "brightness down")
    , ("<XF86MonBrightnessUp>"  , spawn "brightness up")
    , ("<XF86MyComputer>"       , spawn (myTerminal ++ " -e lf-run"))
    , ("<XF86PowerOff>"         , spawn "dm-power")
    , ("<XF86ScreenSaver>", spawn "slock & xset dpms force off; mpc pause; pauseallmpv")
    , ("<XF86Sleep>"            , spawn "sudo -A zzz")
    , ("<XF86TaskPane>"         , spawn (myTerminal ++ " -e btop"))
    , ("<XF86TouchpadOff>"      , spawn "synclient TouchpadOff=1")
    , ("<XF86TouchpadOn>"       , spawn "synclient TouchpadOff=0")
    , ("<XF86TouchpadToggle>", spawn "(synclient | grep 'TouchpadOff.*1' && synclient TouchpadOff=0) || synclient TouchpadOff=1")
    , ("<XF86WWW>"              , spawn "$BROWSER")

    -- Media controls
    , ("<XF86AudioForward>"     , spawn "mpc seek +10")
    , ("<XF86AudioLowerVolume>" , spawn "volume down")
    , ("<XF86AudioMedia>"       , spawn (myTerminal ++ " -e ncmpcpp"))
    , ("<XF86AudioMicMute>"     , spawn "mic-toggle")
    , ("<XF86AudioMute>"        , spawn "volume mute")
    , ("<XF86AudioNext>"        , spawn "mpc next")
    , ("<XF86AudioPause>"       , spawn "mpc pause")
    , ("<XF86AudioPlay>"        , spawn "mpc play")
    , ("<XF86AudioPrev>"        , spawn "mpc prev")
    , ("<XF86AudioRaiseVolume>" , spawn "volume up")
    , ("<XF86AudioRewind>"      , spawn "mpc seek -10")
    , ("<XF86AudioStop>"        , spawn "mpc stop")
    --END_KEYS
    ]
    ++ [
    --START_WHICHKEYS
        -- Cheatsheets
         (leader ++ "h k", spawn "xmonad-whichkeys") -- shotkey applications key binds list
       , (leader ++ "h x", spawn "xmonad-keys") -- xmonad key bindings

        -- Emacs
       , (leader ++ "e e", spawn (myEmacs ++ "--eval '(dashboard-refresh-buffer)'")) -- Emacs dashboard
       , (leader ++ "e b", spawn (myEmacs ++ "--eval '(ibuffer)'")) -- Emacs list buffers
       , (leader ++ "e d", spawn (myEmacs ++ "--eval '(dired nil)'")) -- Emacs dired
       , (leader ++ "e n", spawn (myEmacs ++ "--eval '(elfeed)'")) -- gmacs elfeed rss

        -- Applications
       , (leader ++ "o w", spawn "$BROWSER")
       , (leader ++ "o d", spawn "/usr/bin/discord --no-sandbox")
       , (leader ++ "o e", spawn "/usr/bin/thunderbird")
       , (leader ++ "o f", spawn "/media/ftb/FTBApp")
       , (leader ++ "o o", spawn "env DESKTOPINTEGRATION=false /usr/bin/obsidian --no-sandbox")
       , (leader ++ "o t", spawn "env DESKTOPINTEGRATION=false /usr/bin/todoist --no-sandbox")
       , (leader ++ "o v", spawn "/usr/bin/vscodium")

        -- Dmenu Scripts
       , (leader ++ "p b", spawn "rofi-buku") -- Bookmark manager
       , (leader ++ "p c", spawn "dm-clip") -- Clips clipboard
       , (leader ++ "p d", spawn "dm-directories") -- dmenu directories manager
       , (leader ++ "p e", spawn "dm-emoji") -- Emoji keyboard
       , (leader ++ "p k", spawn "dm-kill") -- Terminate applications
       , (leader ++ "p m", spawn "dm-man") -- Man pages list
       , (leader ++ "p o", spawn "dm-mount") -- Mount drives, including USBs and a Android devices
       , (leader ++ "p p", spawn "rofi-pass") -- Password manager and autotyper
       , (leader ++ "p r", spawn "dm-beats") -- Radio FM
       , (leader ++ "p s", spawn "dm-scripts") -- Find and edit scripts
       , (leader ++ "p u", spawn "dm-umount") -- Unmount any drive
       , (leader ++ "p w", spawn "weatherforecast") -- Display the weather forecast

        -- Music mpd/mpc/ncmpcpp and volume control
       , (leader ++ "m n", namedScratchpadAction myScratchPads "ncmpcpp") -- Ncmpcpp Player
       , (leader ++ "m m", spawn "mic-toggle") -- Toggle mute of microphone

        -- Systems
       , (leader ++ "; a", spawn "setwallpaper a2n")
       , (leader ++ "; d", spawn "setwallpaper dt")
       , (leader ++ "; e", spawn "setwallpaper elyk")
       , (leader ++ "; v", spawn "pavucontrol")
       , (leader ++ "; w", spawn "nsxiv -rqto $XDG_PICTURES_DIR/wallpapers/*")

        -- Default launchers
       , (leader ++ "t"  , spawn myTerminal) -- Spawn alacritty terminal
       , (leader ++ "d"  , spawn "rofi-launcher") -- Rofi launcher
       , (leader ++ "r"  , spawn (myTerminal ++ " -e lf-run")) -- lf file manager

    --END_WHICHKEYS
       ]
       where leader = myLeader

myKeys conf =
  let modm = modMask conf
  in  M.fromList
        $ ((modm .|. shiftMask, xK_b), setLayout $ XMonad.layoutHook conf)
        : [ ((m .|. modm, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5], (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
  [ ((modMask, button1)              , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) --Set the window to floating mode and move by dragging
  , ((modMask, button2)              , \w -> focus w >> windows W.shiftMaster) --Raise the window to the top of the stack
  , ((modMask, button3)              , \w -> focus w >> Flex.mouseResizeWindow w) --Set the window to floating mode and resize by dragging
  , ((modMask, button4)              , const $ moveTo Prev workspaceOnCurrentScreen) --Switch to previous workspace
  , ((modMask, button5)              , const $ moveTo Next workspaceOnCurrentScreen) --Switch to next workspace
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
    . ewmh
    . docks
    . rescreenHook rescreenCfg
    . dynamicSBs myStatusBarSpawner
    $ def { manageHook = insertPosition Master Newer <+> myManageHook <+> namedScratchpadManageHook myScratchPads <+> manageDocks
          , focusFollowsMouse = True
          , clickJustFocuses = False
          , modMask = myModMask
          , terminal = myTerminal
          , startupHook = myStartupHook
          , keys = myKeys
          , mouseBindings = myMouseBindings
          , handleEventHook = myHandleEventHook
          , layoutHook = showWName' myShowWNameTheme myLayoutHook
          , workspaces = myWorkspaces
          , borderWidth = myBorderWidth
          , normalBorderColor = myNormalColor
          , focusedBorderColor = myFocusColor
          , logHook = logHook def <+> updatePointer (0.5, 0.5) (0, 0) <+> masterHistoryHook <+> refocusLastLogHook >> nsHideOnFocusLoss myScratchPads
          }
    `additionalKeysP` myAdditionalKeys
