-- vim: ft=haskell

Config { font = "xft:Iosevka Nerd Font:weight=heavy:pixelsize=18:antialias=true:hinting=true"
       , additionalFonts = [ "xft:monospace:weight=bold:pixelsize=18:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=18"
                           , "xft:Font Awesome 6 Brands:pixelsize=18"
                           ]
       , borderColor      = "black"
       , borderWidth      = 0
       , border           = BottomB
       , textOffset       = -1
       , iconOffset       = -8
       , alpha            = 255
       , position         = BottomSize L 100 30
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
                      Run Date "%a %d %b<fc=#aac0f0> <fn=2>\xF073</fn></fc>%l:%M:%S%p " "date" 1
                    , Run Battery [
                      "-t", "<acstatus> <left>% <fn=1>\xF64F </fn><timeleft>",
                      "--",
                      "-O", "<fn=1>\xF585 </fn>", -- Charging
                      "-o", "<fn=1>\xF58D </fn>", -- Discharning
                      "-i", "<fn=1>\xF1E6 </fn>", -- Idle
                      "-h", "green",
                      "-l", "red"
                      ] 10
                    , Run MPD ["-t",
                               "<title> (<artist>) <statei>",
                               "--", "-P", "<fn=2>\xF144 </fn>", "-Z", "<fn=2>\xF28B </fn>", "-S", "<fn=2>\xF28D </fn>"] 10
                    , Run Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_0"
                    ]
                    
      , template = " <action=`jgmenu_run`><icon=haskell_20.xpm/></action> \
                    \%_XMONAD_LOG_0%}\


                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#5294e2> <action=`sh -c thunderbird`>%date%</action></fc></box>{\


                    \<box type=Bottom width=2 mb=2 color=#bbc2cf><fc=#bbc2cf> <action=`alacritty -e ncmpcpp`><action=`mpc toggle` button=3>%mpd%</action></action></fc></box> \


                    \<box type=Bottom width=2 mb=2 color=#da8548><fc=#da8548> <action=`alacritty -e gotop`>%battery%</action></fc></box>\

                    \%trayerpad%"
                    } 
