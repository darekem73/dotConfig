#monitor single
nitrogen --restore &
conky -d -c ~/.conkyrc-stat
#nm-applet &
#blueman-applet &
#pamac-tray &
#system-config-printer-applet &
#volumeicon &
#xfce4-power-manager &
#unclutter -idle 1 -root &
#mopidy &
#compton &
#picom &
setxkbmap -option caps:escape
xset r rate 200 40
slstatus &
#synclient TapButton1=1 TapButton2=3 TapButton3=2
#synclient RightButtonAreaLeft=0 RightButtonAreaTop=0
#xautolock -time 10 -locker slock

# if [[ `xrandr | egrep connected | grep -v dis | wc -l` -eq 2 ]]; then 
# 	monitor extend
# else
# 	monitor single
# fi
