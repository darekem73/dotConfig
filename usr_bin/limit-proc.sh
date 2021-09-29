#!/bin/bash

usage() {
	echo "Usage: <$0> <process_name> [CORE-LIST]"
}

MYNAME=$(echo $0 | cut -d\/ -f2)

if [[ "$1" == "--help" ]]; then
	usage
	exit 0
elif [[ $# -eq 1 ]]; then
	PROCESS=$1
	CORES=""
elif [[ $# -eq 2 ]]; then
	PROCESS=$1
	CORES="$2"
else
	usage
	exit 0
fi

#ps -eo pid,ni,pri,comm | grep -i chrome | grep -v grep

if [[ "$1" == "" ]]; then
	ps -eo pid,ni,pri,cmd | grep -i $PROCESS | egrep -iv "grep|$MYNAME" | awk '{print $1}' | xargs -I{} taskset -acp {}
else
	ps -eo pid,ni,pri,cmd | grep -i $PROCESS | egrep -iv "grep|$MYNAME" | awk '{print $1}' | xargs -I{} taskset -acp $CORES {}
fi
