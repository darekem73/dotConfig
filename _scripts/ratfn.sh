#!/bin/bash
CONF_DIR=.config/alacritty

ARG="$1"
function usage() {
	echo "Usage: <$0> { \"<font_name>\" }"
}

if [[ -z "$ARG" ]]; then
	usage
	exit 0
fi

atc.rb --config $HOME/$CONF_DIR/alacritty.yml --font-name "$1"
