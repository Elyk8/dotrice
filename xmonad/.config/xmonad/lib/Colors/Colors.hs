{-# LANGUAGE ScopedTypeVariables #-}

module Colors.Colors ( basebg
                     , basefg
                     , basecr
                     , base00
                     , base01
                     , base02
                     , base03
                     , base04
                     , base05
                     , base06
                     , base07
                     , base08
                     , base09
                     , base10
                     , base11
                     , base12
                     , base13
                     , base14
                     , base15
                     , colorTrayer
                     ) where

import Colors.Xresources (xprop)
import XMonad

basebg, basefg, base00, base01, base02, base03, base04, base05, base06, base07, base08, base09, base10, base11, base12, base13, base14, base15, colorTrayer :: String
colorTrayer = xprop "trayerColor"
basebg = xprop "*background"
basefg = xprop "*foreground"
basecr = xprop "*cursorColor"
base00 = xprop "*color0"
base01 = xprop "*color1"
base02 = xprop "*color2"
base03 = xprop "*color3"
base04 = xprop "*color4"
base05 = xprop "*color5"
base06 = xprop "*color6"
base07 = xprop "*color7"
base08 = xprop "*color8"
base09 = xprop "*color9"
base10 = xprop "*color10"
base11 = xprop "*color11"
base12 = xprop "*color12"
base13 = xprop "*color13"
base14 = xprop "*color14"
base15 = xprop "*color15"
