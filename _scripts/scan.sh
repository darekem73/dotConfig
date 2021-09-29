#!/bin/bash

for f in `seq 1 254`; do
        ping 192.168.1.$f -c 1 -w 1 | awk 'BEGIN{FPAT="192.168.1.[0-9]+:|time=[0-9]+.*[0-9]* ms"}; NF>0{print $1, $2}' &
done

