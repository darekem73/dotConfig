#!/bin/bash
if [[ -z "$1" ]]; then
	echo "Usage: <$0> <file.pdf> [fullscreen]"
	exit
fi

if [[ -z "$2" ]]; then
	impressive -f -g 1920x1080 $1
else
	impressive $1
fi
