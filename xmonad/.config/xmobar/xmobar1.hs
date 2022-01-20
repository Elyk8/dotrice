-- vim: ft=haskell

Config { font = "xft:Ubuntu:weight=bold:pixelsize=13:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Iosevka Nerd Font:weight=heavy::pixelsize=15:antialias=true:hinting=true"
                           , "xft:Font Awesome 5 Free Solid:pixelsize=15"
                           , "xft:Font Awesome 5 Brands:pixelsize=15"
                           ]
       , bgColor          = "#1E1E1E"
       , fgColor          = "#569CD6"
       , borderColor      = "black"
       , borderWidth      = 1
       , border           = BottomB
       , textOffset       = -1
       , iconOffset       = -8
       , alpha            = 255
       , position         = TopSize L 100 24
       , lowerOnStart     = True
       , hideOnStart      = False
       , overrideRedirect = True
       , allDesktops      = True
       --, pickBroadest     = True
       , persistent       = True       
       , sepChar          = "%"
       , alignSep         = "}{"
       , iconRoot         = ".config/xmobar/icons"
       , commands         =
                    [
                      Run Network "wlps7" ["-t", "<fn=1>\xfd3f</fn> <dev>"] 20 
                    , Run Date "%l:%M:%S:%p " "date" 1
                    , Run Com "uname" ["-r"] "" 3600
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_1"
                    ]
                    
       , template = " <action=`jgmenu_run`><icon=haskell_20.xpm/></action> \
                    \ <fc=#666666>|</fc> \
                    \ <fn=1>%_XMONAD_LOG_1%</fn>}{\


                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#aac0f0><fn=3> </fn>  <action=`st -e gotop`>%uname%</action></fc></box>    \
                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#5294e2> <action=`sh -c thunderbird`>%date%</action></fc></box>"
                    }
