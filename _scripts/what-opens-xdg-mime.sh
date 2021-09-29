#!/bin/bash

function help() {
	echo "Findout what is default application for given extension"
	echo "Usage: <$0> <extension>"
	echo "Example: <$0> pdf"
}

[ -z "$1" ] && help && exit 0
ff=test.$1 && xdg-mime query default $(touch /tmp/$ff && xdg-mime query filetype /tmp/$ff) && rm /tmp/$ff
