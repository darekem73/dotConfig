#monitor single
#nitrogen --restore &
#conky -d -c ~/.conkyrc-stat
#nm-applet &
#blueman-applet &
#pamac-tray &
#system-config-printer-applet &
#volumeicon &
#xfce4-power-manager &
#unclutter -idle 1 -root &
#mopidy &
#compton &
#setxkbmap -option caps:escape
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape']"
#xset r rate 200 40
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 40
gsettings set org.gnome.desktop.peripherals.keyboard delay 200

#slstatus &
#synclient TapButton1=1 TapButton2=3 TapButton3=2
#xautolock -time 10 -locker screenlock

#if [[ `xrandr | egrep connected | grep -v dis | wc -l` -eq 2 ]]; then 
#	monitor extend
#else
#	monitor single
#fi
