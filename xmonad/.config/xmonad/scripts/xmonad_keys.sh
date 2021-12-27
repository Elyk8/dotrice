#!/usr/bin/env bash
set -euo pipefail


sed -n '/START_KEYS/,/END_KEYS/p' ~/.config/xmonad/xmonad.hs | \
    grep -e ', ("' \
    -e '\[ (' \
    -e 'KB_GROUP' | \
    grep -v '\-\- , ("' | \
    sed -e 's/^[ \t]*//' \
    -e 's/, (/(/' \
    -e 's/\[ (/(/' \
    -e 's/-- KB_GROUP /\n/' \
    -e 's/", /"\t: /' | \
    yad --text-info --back=#282a33 --fore=#5294e2 --geometry=1200x800

