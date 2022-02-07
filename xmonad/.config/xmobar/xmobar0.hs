-- vim: ft=haskell

Config { font = "xft:Ubuntu:weight=bold:pixelsize=16:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Iosevka Nerd Font:weight=heavy:pixelsize=18:antialias=true:hinting=true"
                           , "xft:Font Awesome 5 Free Solid:pixelsize=16"
                           , "xft:Font Awesome 5 Brands:pixelsize=16"
                           ]
       , bgColor          = "#1E1E1E"
       , fgColor          = "#569CD6"
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
       , pickBroadest     = True
       , persistent       = True       
       , sepChar          = "%"
       , alignSep         = "}{"
       , iconRoot         = ".config/xmobar/icons"
       , commands         =
                    [	                  
                      Run Date " %a %d %b  <fc=#aac0f0> <fn=2>\xF073 </fn></fc> %l:%M:%S %p " "date" 1
                    , Run MPD ["-t",
                               "<title> (<artist>) <statei> [<flags>]",
                               "--", "-P", "<fn=2>\xF144 </fn>", "-Z", "<fn=2>\xF28B </fn>", "-S", "<fn=2>\xF28D </fn>"] 10
                    , Run Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_0"
                    ]
                    
      , template = " <action=`jgmenu_run`><icon=haskell_20.xpm/></action> \
                    \ <fn=1>%_XMONAD_LOG_0%</fn>}\


                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#5294e2> <action=`sh -c thunderbird`>%date%</action></fc></box>{\


                    \<box type=Bottom width=2 mb=2 color=#bbc2cf><fc=#bbc2cf> <action=`alacritty -e ncmpcpp`><action=`mpc toggle` button=3>%mpd%</action></action></fc></box>\

                    \%trayerpad% "
                    } 
