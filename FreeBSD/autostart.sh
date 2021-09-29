xrandr --output VGA-0 --auto
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
compton &
setxkbmap -option caps:escape
xset r rate 200 40
#slstatus &
while true; do xsetroot -name "`uptime | sed 's/.*,//'` | `date '+%Y-%m-%d %H:%M:%S'`"; sleep 1; done &
#synclient TapButton2=3 TapButton3=2
#XAUTOLOCK -time 10 -locker screenlock
