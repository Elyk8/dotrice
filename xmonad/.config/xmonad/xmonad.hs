------------------------------------------
---              Imports               ---
------------------------------------------

{-# LANGUAGE LambdaCase #-}
import Colors.Colors
import qualified Data.Map as M
import Data.List
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Tree
import Control.Monad
import System.Directory
import System.IO (hPutStrLn)
import System.Posix.Files
import System.Exit (exitSuccess)

import XMonad
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.FloatKeys
import XMonad.Actions.GroupNavigation
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.OnScreen
import XMonad.Actions.Promote
import XMonad.Actions.MouseResize
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo (runOrRaise)

import XMonad.Actions.WorkspaceNames
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition -- set turn tile
import XMonad.Hooks.ManageDocks (docks, avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, doFullFloat, doSideFloat, isDialog, isFullscreen, isInProperty, transience', (-?>), Side(CE))
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (dynamicSBs, dynamicEasySBs, StatusBarConfig, statusBarPropTo,statusBarProp, withSB)
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory --(workspaceHistoryHook)

import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))

import XMonad.Prompt (XPConfig (..),XPPosition (Top),defaultXPKeymap,deleteAllDuplicates)
import XMonad.Prompt.ConfirmPrompt  (confirmPrompt)
import XMonad.Prompt.FuzzyMatch  (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.OrgMode (orgPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties

import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Util.Hacks as Hacks
import qualified XMonad.StackSet as W --hiding (focusMaster, workspaces)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

myFont :: String
myFont = "xft:Ubuntu Nerd Font:regular:size=9:antialias=true:hinting=true"
myBoldFont = "xft:JetBrains Mono Nerd Font:medium:size=40"

myModMask :: KeyMask
myModMask = mod4Mask 

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "brave" 

myEditor :: String
myEditor = "lvim"

myBorderWidth :: Dimension
myBorderWidth = 3 

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
    spawnOnce "~/.config/xmonad/scripts/autostart.sh"

------------------------------------------
---               Theme                ---
------------------------------------------

myTabTheme = def 
           { activeColor         = basebg
           , inactiveColor       = basebg
           , urgentColor         = base12
           , activeBorderColor   = base05
           , inactiveBorderColor = base11
           , urgentBorderColor   = base04
           , activeTextColor     = base10
           , inactiveTextColor   = base13
           , urgentTextColor     = base04
           , fontName            = myFont
           }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font             = myBoldFont 
    , swn_fade             = 0.6
    , swn_bgcolor          = basebg
    , swn_color            = base05
    }

myXPConfig = def
            { font                = myFont
            , fgColor             = basefg
            , bgColor             = basebg
            , borderColor         = base05
            , promptBorderWidth   = 0
            , bgHLight            = basebg  
            , fgHLight            = base13
            --, position          = CenteredAt 0.3 0.5
            , position            = Top
            , alwaysHighlight     = True           -- Disables tab cycle
            , height              = 24
            , maxComplRows        = Just 10        -- set to 'Just 5' for 5 rows
            , historySize         = 50
            , historyFilter       = deleteAllDuplicates
            , defaultText         = []
            , autoComplete        = Just 500000
            , showCompletionOnTab = False          -- False means auto completion
            , searchPredicate     = fuzzyMatch
            , sorter              = fuzzySort
                 
            }

toggleFloat x w = windows (\s -> if M.member w (W.floating s) then W.sink w s
                                 else
                                     if x == R then (W.float w (W.RationalRect 0.5 0.015 0.5 1.0) s)
                                     else (W.float w (W.RationalRect 0.0 0.015 0.5 1.0) s))

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs (isOnScreen <$> currentScreen)

------------------------------------------
---               Main                 ---
------------------------------------------

main :: IO ()
main = do
    xmonad . withSB myBars . ewmh . docks . ewmhFullscreen . dynamicEasySBs barSpawner $ def
       { manageHook          = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , handleEventHook    = Hacks.trayerAboveXmobarEventHook <+> swallowEventHook (className =? "Alacritty") (return True)
        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalColor
        , focusedBorderColor = myFocusColor
       }`additionalKeysP` myKeys

------------------------------------------
---               Xmobar               ---
------------------------------------------

myBars :: StatusBarConfig
myBars = (xmobar0 <> xmobar1) -- <> xmobar2

xmobar0 = statusBarPropTo "_XMONAD_LOG_0" "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0" (myXmobarPP)
xmobar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1" (myXmobarPP)
-- xmobar2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 0 $HOME/.config/xmobar/xmobarrc2" (myXmobarPP)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure $ xmobar0  
barSpawner 1 = pure $ xmobar1
-- barSpawner 2 = pure $ xmobar2
barSpawner _ = mempty 

myXmobarPP :: X PP
myXmobarPP = pure . filterOutWsPP [scratchpadWorkspaceTag] $ def                                                   
          { ppCurrent = xmobarColor base05 "" . \s -> "\61713 "
          , ppVisible = xmobarColor base05 "" . \s -> clickable s "\61708 "
          , ppHidden = xmobarColor basefg "" . \s -> clickable s "\61842 "
          , ppHiddenNoWindows = xmobarColor base05 ""  . \s -> clickable s "\61708 "
          , ppTitle = xmobarColor basefg "" . shorten 60
          , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
          , ppUrgent = xmobarColor base05 "" . wrap "!" "!"
          , ppExtras  = [windowCount]
          , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
          }

------------------------------------------
---             WorkSpaces             ---
------------------------------------------
--myWorkspaces = ["\61728 ", "\62057 ", "\61501 ", "\61564 ", "\61461 ", "\61787 ", "\61550 ", "\59154 ", "\61475 "]
myWorkspaces = ["\62057 ", "\61564 ", "\59333 ", "\63434 ", "\61573 "]

clickable :: String -> String -> String
clickable s us = "<action=xdotool key super+" ++ (transform_ws s) ++ ">" ++ us ++ "</action>"

transform_ws :: String -> String
transform_ws "\62057 " = "1"
transform_ws "\61564 " = "2"
transform_ws "\59333 " = "3"
transform_ws "\63434 " = "4"
transform_ws "\61573 " = "5"
--transform_ws "\61787 " = "6"
--transform_ws "\61550 " = "7"
--transform_ws "\59154 " = "8"
--transform_ws "\61475 " = "9"

------------------------------------------
---              Layouts               ---
------------------------------------------

myLayoutHook =
        avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats 
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =  withBorder myBorderWidth tall
      ||| taller
        ||| noBorders monocle
          ||| floats
            ||| noBorders tabs
              ||| grid
                ||| spirals
                  ||| threeCol
                    ||| threeRow
                      ||| tallAccordion
                        ||| wideAccordion

tall =
  renamed [Replace "tall"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 12 $
              mySpacing 8 $
                ResizableTall 1 (3/100) (1/2) []
           
taller =
  renamed [Replace "taller"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            magnifier $
              limitWindows 12 $
                mySpacing 8 $
                  ResizableTall 1 (3/100) (1/2) []

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
                  Grid (16/10)

spirals =
  renamed [Replace "spirals"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            mySpacing' 8 $ 
              spiral (6/7)

threeCol =
  renamed [Replace "threeCol"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 7 $
              ThreeCol 1 (3/100) (1/2)

threeRow =
  renamed [Replace "threeRow"] $
    avoidStruts $
      smartBorders $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 7 $
              Mirror $
                ThreeCol 1 (3/100) (1/2)

floats =
  renamed [Replace "floats"] $
    avoidStruts $
      smartBorders $
        limitWindows 20 simplestFloat

tabs =
  renamed [Replace "tabs"] $
    tabbed shrinkText myTabTheme

tallAccordion =
  renamed [Replace "tallAccordion"] $
    Accordion

wideAccordion =
  renamed [Replace "wideAccordion"] $
    Mirror Accordion

------------------------------------------
---            Window Rules            ---
------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "Brave-browser"                               --> doShift ( myWorkspaces !! 0 )
     , className =? "Mozzilla Firefox"                            --> doShift ( myWorkspaces !! 0 )
     , className =? "Thunar"                                      --> doShift ( myWorkspaces !! 1 )
     , className =? "Gimp-2.10"                                   --> doShift ( myWorkspaces !! 2 )
     , className =? "Virt-manager"                                --> doShift ( myWorkspaces !! 3 )
     , className =? "Pamac-manager"                               --> doShift ( myWorkspaces !! 4 )
     , className =? "Pycalendar.py"                               --> doCenterFloat
     , className =? "notification"                                --> doCenterFloat
     , className =? "Yad"                                         --> doCenterFloat
     , className =? "Gcolor3"                                     --> doCenterFloat
     , className =? "Xfce4-power-manager-settings"                --> doCenterFloat
     , className =? "Nwg-drawer"                                  --> doFullFloat
     , title =? "scratchpad"                                      --> doFloat
     , title =? "ncmpcpp"                                         --> doFloat
     , className =? "confirm"                                     --> doFloat
     , className =? "file_progress"                               --> doFloat
     , className =? "dialog"                                      --> doFloat
     , className =? "download"                                    --> doFloat
     , className =? "error"                                       --> doFloat
     , className =? "toolbar"                                     --> doFloat
     , className =? "Gmrun"                                       --> doFloat
     , stringProperty "WM_NAME" =? "LINE"                         --> doFloat
     , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"  --> doFloat
     , stringProperty "WM_ICON_NAME" =? "Gvim        "            --> doFloat
     , stringProperty "WM_ICON_NAME" =? "Launch Application"      --> doFloat
     , (className =? "firefox" <&&> resource =? "Dialog")         --> doFloat  
     , isFullscreen -->  doFullFloat
     ]

------------------------------------------
---            ScratchPads             ---
------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
      NS "ncmpcpp"              launchNcmpcpp          (title =? "ncmpcpp")    (customFloating $ W.RationalRect h w t l)
    , NS "terminal"             launchTerminal         (title =? "scratchpad") (customFloating $ W.RationalRect h w t l)
  ]
  where
    launchNcmpcpp  = myTerminal ++ " -t ncmpcpp -e ncmpcpp"
    launchTerminal = myTerminal ++ " -t scratchpad"
    h = 0.8
    w = 0.8
    t = 0.95 -h
    l = 0.95 -w

------------------------------------------
---                Keys                ---
------------------------------------------
-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP Xmonad
        [ ("M-C-r", spawn "xmonad --restart; killall xmobar")
        , ("M-S-<Esc>", io exitSuccess)

    -- KB_GROUP Kill windows
        , ("M-q", kill)     -- Kill the currently focused client
        , ("M-S-q", killAll)   -- Kill all windows on current workspace

    -- KB_GROUP Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        --, ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        -- , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- KB_GROUP dual monitor swicher
        , ("M-w", onNextNeighbour def W.view >> warpToWindow (1%2) (1%2))
        , ("M-S-w", onNextNeighbour def W.shift >> warpToWindow (1%2) (1%2))
        , ("M-b", onPrevNeighbour def W.view >> warpToWindow (1%2) (1%2))
        , ("M-S-b", onPrevNeighbour def W.shift >> warpToWindow (1%2) (1%2))

    -- KB_GROUP Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-C-s", sinkAll)                       -- Push ALL floating windows to tile

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- KB_GROUP Windows navigation
        , ("M-n", windows W.focusMaster >> warpToWindow (1%2) (1%2))  -- Move focus to the master window
        , ("M-j", windows W.focusDown >> warpToWindow (1%2) (1%2))    -- Move focus to the next window
        , ("M-k", windows W.focusUp >> warpToWindow (1%2) (1%2))      -- Move focus to the prev window
        , ("M-S-n", windows W.swapMaster)                             -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)                               -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)                                 -- Swap focused window with prev window
        , ("M-<Backspace>", promote)                                  -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)                                -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)                                   -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-<Space>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Tab>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full    
   
    -- KB_GROUP Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- KB_GROUP Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width      

    -- KB_GROUP Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- KB_GROUP Scratchpad windows
        , ("M-s n", namedScratchpadAction myScratchPads "ncmpcpp")  -- Ncmpcpp Player
        , ("M-s t", namedScratchpadAction myScratchPads "terminal") -- Terminal    
   ]
-- END_KEYS   
