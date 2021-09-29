#!/bin/bash

nthCore=$(lscpu|grep node0|cut -d"-" -f2)

for i in /sys/devices/system/cpu/cpu0/cpufreq/{cpuinfo,scaling}_*; do
  pname=$(basename $i)
  [[ "$pname" == *available* ]] || [[ "$pname" == *transition* ]] || \
  [[ "$pname" == *driver* ]]    || [[ "$pname" == *setspeed* ]] && continue
  echo "$pname: "
  for j in `seq 0 $nthCore`;do
    kparam=$(echo $i | sed "s/cpu0/cpu$j/")
    sudo cat $kparam
  done
done | paste - - - - - - - - - | column -t

