#!/bin/bash
xprop -root | grep -v NET_SUPPORT | awk -F=\  '/CURRENT_DESKTOP/ {cd=$2+1}; /NUMBER_OF_DESKTOPS/ {nd=$2}; END {for (f=1;f<=nd;f++) {if (f==cd) printf "|%s| ", f; else printf "%s ", f} }'
winid=`xprop -root | grep ACTIVE_WINDOW | sed -n 1p | awk -F#\  '{printf "%s", $2}'`
if [[ "$winid" != "0x0" ]]; then 
	xprop -id $winid | grep WM_NAME | sed -n 1p | awk -F=\  '{printf "%s", $2}'
else
	awk 'BEGIN {printf "%s", ""}'
fi
