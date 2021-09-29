#! /bin/bash
echo $(date)

cat /proc/cpuinfo | grep MHz | egrep -o "[0-9]+\.[0-9]+"

for p in 0 1 2 3; do
        (( f=$(cat /sys/devices/system/cpu/cpu$p/cpufreq/scaling_cur_freq) / 1000 ))
        echo $f
done

for h in 0 1 2 3 4 5 6; do
        (( temp=$(cat /sys/class/thermal/thermal_zone$h/temp) / 1000 ))
        for t in 1 2 3 4 5; do
                NAME=/sys/class/hwmon/hwmon$h/name
                LABEL=//sys/class/hwmon/hwmon$h/temp$t"_label"
                FILE=/sys/class/hwmon/hwmon$h/temp$t"_input"
                [ -f $FILE ] && [ -f $NAME ] && echo $(cat $NAME) $([ -f $LABEL ] && cat $LABEL) $(cat $FILE) $temp
        done
done

