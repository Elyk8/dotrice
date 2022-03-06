#!/usr/bin/env bash
##########################################################
## Look through ~/.xmonad/xmonad.hs for the given key submap
## in EZ-Config format.
## Parse each line for the keys, commands and trailing comments.
## Use comments for the description when available.
## format into columns to fit the screen width.
## send to dzen2 along with font colors and font.
##########################################################
KEYMAP=$1
# make FW bigger if the columns don't fit on your screen
FW=450
LH=${10}
X=$2
W=$4
KEYCOLOR=$6
ARROWCOLOR=$7
CMDCOLOR=$8
FONT=$9
COLS=$((W / FW))

INFO=$(awk -v cmdcolor="$CMDCOLOR" -v keycolor="$KEYCOLOR" -v arrowcolor="$ARROWCOLOR" -v cols=$COLS \
           'BEGIN {nr=0}
            /^'"$KEYMAP"'/,/^\s*\].*$/ {
                # any comments will be replaced by an empty line
                if ($0 ~ /^\s*--+/) next

                if (nr == 0) {
                    split($0, firstline, " --", seps)
                    if (length(firstline[2]) > 0){
                        label=firstline[2]
                    } else {
                        label=$1
                    }
                }
                nr=nr+1

                # get the key entry and any following comment.
                split($0, splitline, " --", seps)
                comma_loc=index(splitline[1], "\",")
                match(substr(splitline[1], 1, comma_loc-1), /^.*\"(.*)/, keys)
                match(substr(splitline[1], comma_loc+2), /^ *(.*)\)/, command)

                # remove any leading spaces from the comment
                gsub(/^\S\+/, "", splitline[2])
                gsub(/<Space>/, "SPC", keys[1])
                gsub(/ /, "", keys[1])

                # skip any empty records.
                if (length(command[1]) > 0){
                    # if there is a comment use that for the description.
                    if (length(splitline[2]) > 0) {
                            desc=splitline[2]
                        } else {
                            desc=command[1]
                    }
                key_hint[i++] = sprintf (" ^fg(%s)%3s ^fg(%s)->^fg(%s)%-30.30s", keycolor, keys[1], arrowcolor, cmdcolor, desc)
                } else if (nr > 1) { # skip the first line
                    key_hint[i++] = sprintf (" ^fg(%s)%3s ^fg(%s)  ^fg(%s)%-30.30s", "", "", "", "", "")
                }
            }
            END {
                print "^fg("keycolor")"label
                print " "
                rows = int( ((i+1) / cols) )
                for (j=0; j<=i;) {
                    for (k=0; k < rows; k++) {
                         row[k] = row[k] key_hint[j++]
                    }
                }
                for (k=0; k <= rows; k++) {print row[k]}
                print ""
            }' \
                ~/.config/xmonad/xmonad.hs)

echo "$INFO"

N_LINES=$(wc -l <<< "$INFO")
Y=$(($3 + $5 - (LH * (N_LINES+1))))
# sleep 0.1
# $KEYMAP ($2 , $3 , $4 , $5, $LH, $X, $Y, $W, $N_LINES, $COLS
(echo "$INFO"; cat) | dzen2 -l $((N_LINES)) -fn "${FONT}" -h "$LH" -x "$X" -y "$Y" -w "$W" -e onstart=uncollapse
