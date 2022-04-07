local module = {}

-- Color theme: options are:
-- "light", "dark", "mirage" and "xrdb"
module.color_scheme     = "xrdb"
--module.font             = "InputSans Nerd Font"
<<<<<<< HEAD
-- module.font             = "DejaVu Sans Bold"
module.font             = "Mononoki Nerd Font"
module.icon_font        = "Mononoki Nerd Font"
module.notify_font      = "Mononoki Nerd Font"
=======
module.font             = "DejaVu Sans Bold"
module.icon_font        = "InputSans Nerd Font"
module.notify_font      = "InputSans Nerd Font"
>>>>>>> dc98d66 (commit)
module.tasklist_font    = "DejaVu Sans"
--module.icon_font      = "Font Awesome 5 Pro Bold "

-- Use gaps?
<<<<<<< HEAD
module.gap              = 5
=======
module.gap              = 2
>>>>>>> dc98d66 (commit)

-- Your city for the weather forecast widget
-- units can be either:
-- "metric" (celsius)
-- "imperial" (farenheight)
module.city_id          = 5393212
<<<<<<< HEAD
module.units            = "metric"
=======
module.units            = "imperial"
>>>>>>> dc98d66 (commit)

-- 0 to use font-awesome glyps
-- 1 to use icons from lain lib
module.bat_icons        = 1
module.wifi_icons       = 1
module.volume_icons     = 1
module.weather_icons    = 1

-- Battery module, optional
-- `ls /sys/class/power_supply/`
module.adapter          = "ADP1"
module.battery          = "BAT0"
-- For multiple batteries use:
--module.batteries        = { "BAT0" }

-- Set resource for temperature widget
module.tempfile      = '/sys/devices/platform/coretemp.0/hwmon/hwmon4/temp1_input'

-- Wallpaper path
module.wallpaper_path   = '~/Pictures/Wallpapers/Matcha-mirror.jpeg'

return module
