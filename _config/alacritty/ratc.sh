#!/bin/bash
CONF_DIR=.config/alacritty

ARG="$1"
function usage() {
	echo "Usage: <$0> { <preset_number> | --auto | --random | --next | --prev | --choice | --restore | --reset }"
}

if [[ -z "$ARG" ]]; then
	usage
	exit 0
fi

if [[ "$ARG" == "--auto" ]]; then
	PICOM=$([[ `ps -elf | grep -v grep | egrep picom 2>/dev/null` ]] && echo YES)
	ARG=`cat $0 | egrep -o "[0-9][0-9])|SCHEME=.*\.yml" | awk -v seed=$RANDOM -v PICOM=$PICOM 'BEGIN{srand(seed)}; /[0-9]{2}/ {num=substr($0,1,2); getline name; if (num~/1[0-9]/) {t[num]=name} else if (num~/2[0-9]/) {o[num]=name} }; END {r=rand(); if (PICOM=="YES") {i=int(r*(length(t))); scheme=t[10+i]} else {i=int(r*(length(o))); scheme=o[20+i]}; sub(/SCHEME=/,"",scheme); print scheme}'`
	ARG=$HOME/$CONF_DIR/$ARG
fi

if [[ -f "$ARG" ]]; then
	SCHEME=themes/`basename "$ARG"`
elif [[ "$ARG" =~ [0-9]+ ]]; then
	case "$ARG" in
		0)
			#My default scheme
			SCHEME=themes/my_color_scheme.yml
			;;
		10)
			#Transparent schemes
			SCHEME=themes/my_color_scheme.yml
			;;
		11)
			SCHEME=themes/Kitty.yml
			;;
		12)
			SCHEME=themes/Gnometerm.yml
			;;
		13)
			SCHEME=themes/Hyper.yml
			;;
		20)
			#Opaque schemes
			SCHEME=themes/Tartan.yml
			;;
		21)
			SCHEME=themes/Afterglow.yml
			;;
		22)
			SCHEME=themes/Cobalt-2.yml
			;;
		23)
			SCHEME=themes/Darkside.yml
			;;
		24)
			SCHEME=themes/Gjm.yml
			;;
		25)
			SCHEME=themes/Google.dark.yml
			;;
		26)
			SCHEME=themes/Gruvbox-Dark.yml
			;;
		27)
			SCHEME=themes/Oxide.yml
			;;
		28)
			SCHEME=themes/NumixDarkest.yml
			;;
		*)
			echo "[ERROR] Wrong preset: $ARG"
			echo "Available presets:"
			cat $0 | egrep -o "[0-9][0-9])|SCHEME=.*\.yml" | awk '/[0-9]{2}/ {printf "%s ", $0; getline; print $0}'
			exit 1

	esac
elif [[ "$ARG" == "--random" ]]; then
	SCHEME="`find themes/ -type f | shuf | head -1`"
elif [[ "$ARG" == "--choice" ]]; then
	SCHEME=themes/`ls -1 $HOME/$CONF_DIR/themes | dmenu -l 20`
elif [[ "$ARG" == "--next" ]]; then
	if [[ -f "$HOME/$CONF_DIR/current_scheme.txt" ]]; then
		SCHEME=$(ls -1 $HOME/$CONF_DIR/themes/*.yml | awk -v CUR="`cat $HOME/$CONF_DIR/current_scheme.txt`" 'BEGIN {split(CUR,tab,"/"); CURRENT=tab[length(tab)]; c=0}; {c++; a[c]=$0}; $0~CURRENT {d=c}; END {if (d>=length(a)) {print a[1]} else {print a[d+1]}}')
		SCHEME=themes/`basename "$SCHEME"`
	else
		SCHEME=themes/`ls -1 $HOME/$CONF_DIR/themes | head -1`
	fi
elif [[ "$ARG" == "--prev" ]]; then
	if [[ -f "$HOME/$CONF_DIR/current_scheme.txt" ]]; then
		SCHEME=$(ls -1 $HOME/$CONF_DIR/themes/*.yml | awk -v CUR="`cat $HOME/$CONF_DIR/current_scheme.txt`" 'BEGIN {split(CUR,tab,"/"); CURRENT=tab[length(tab)]; c=0}; {c++; a[c]=$0}; $0~CURRENT {d=c}; END {if (d<=1) {print a[length(a)]} else {print a[d-1]}}')
		SCHEME=themes/`basename "$SCHEME"`
	else
		SCHEME=themes/`ls -1 $HOME/$CONF_DIR/themes | head -1`
	fi
elif [[ "$ARG" == "--restore" ]]; then
	SCHEME=themes/my_color_scheme.yml
elif [[ "$ARG" == "--reset" ]]; then
	rm -f $HOME/$CONF_DIR/current_scheme.txt
	exit 0
else
	echo "[ERROR] File does not exist: $HOME/$CONF_DIR/$SCHEME"
	usage
	exit 1
fi

if [ -f "$HOME/$CONF_DIR/$SCHEME" ]; then
	echo $SCHEME > $HOME/$CONF_DIR/current_scheme.txt
	atc.rb --config $HOME/$CONF_DIR/alacritty.yml --colors "$HOME/$CONF_DIR/$SCHEME"
else
	echo "[ERROR] File does not exist: $HOME/$CONF_DIR/$SCHEME"
	usage
	exit 1
fi
