-- vim: ft=haskell

Config { font = "xft:Ubuntu:weight=bold:italic:pixelsize=16:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Iosevka Nerd Font:weight=bold::pixelsize=16:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=16"
                           , "xft:Font Awesome 6 Brands:pixelsize=16"
                           , "xft:Iosevka Nerd Font:weight=heavy:italic:pixelsize=16:antialias=true:hinting=true"
                           ]
       , borderColor      = "black"
       , borderWidth      = 1
       , border           = BottomB
       , textOffset       = -1
       , iconOffset       = -8
       , alpha            = 255
       , position         = TopSize L 100 30
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
                    , Run Date "%l:%M:%S:%p" "date" 1
                    , Run Com "uname" ["-r"] "" 3600
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_1"
                    ]
                    
       , template = " %_XMONAD_LOG_1%}{\


                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#aac0f0><fn=3> </fn><action=`alacritty -e gotop`>%uname%</action></fc></box>   \

                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#5294e2><action=`sh -c thunderbird`>%date%</action></fc></box>"
                    }
