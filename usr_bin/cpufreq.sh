#!/bin/bash
cat /proc/cpuinfo | grep MHz | egrep -o '[0-9]+\.[0-9]+' | awk '{a+=$0; c++}; END {printf "%d", a/c}'
