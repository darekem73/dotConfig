#!/bin/bash

while true; do 
	gsettings set org.gnome.desktop.background picture-uri file:///$(du -a /usr/share/backgrounds/ | egrep ".jp[e]*g|.png" | awk '{ print $2 }' | shuf | head -1)
	sleep 600
done
