if [ -z "$1" ]; then
	echo "ERROR: enter device number; id=?"
	xinput
else
	xinput map-to-output $1 HDMI1
fi
