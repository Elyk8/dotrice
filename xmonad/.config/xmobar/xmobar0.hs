-- vim: ft=haskell

Config { font = "xft:Ubuntu:weight=bold:italic:pixelsize=18:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Iosevka Nerd Font:weight=heavy:pixelsize=20:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=20"
                           , "xft:Font Awesome 6 Brands:pixelsize=18"
                           , "xft:Iosevka Nerd Font:weight=heavy:italic:pixelsize=20:antialias=true:hinting=true"
                           ]
       , borderColor      = "black"
       , borderWidth      = 0
       , border           = BottomB
       , textOffset       = -1
       , iconOffset       = -8
       , alpha            = 255
       , position         = BottomSize L 100 32
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
                      "-t", "<acstatus><left>% <fn=2>\xF017 </fn><timeleft>",
                      "--",
                      "-O", "<fn=2>\xF5E7 </fn>", -- Charging
                      "-o", "<fn=2>\xF242 </fn>", -- Discharning
                      "-i", "<fn=2>\xF1E6 </fn>", -- Idle
                      "-h", "green",
                      "-l", "red"
                      ] 10
                    , Run MPD ["-t",
                               "<title><statei>",
                               "--", "-P", " <fn=2>\xF144 </fn>", "-Z", " <fn=2>\xF28B </fn>", "-S", "<fn=2>\xF28D </fn>"] 10
                    , Run Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_0"
                    ]
                    
      , template = " %_XMONAD_LOG_0%}\


                    \<box type=Bottom width=2 mb=2 color=#51afef><fc=#5294e2> <action=`sh -c thunderbird`>%date%</action></fc></box>{\


                    \<box type=Bottom width=2 mb=2 color=#bbc2cf><fc=#bbc2cf> <action=`st -e ncmpcpp`><action=`mpc toggle` button=3>%mpd%</action></action></fc></box>   \


                    \<box type=Bottom width=2 mb=2 color=#da8548><fc=#da8548> <action=`st -e gotop`>%battery%</action></fc></box> \

                    \%trayerpad%"
                    } 
