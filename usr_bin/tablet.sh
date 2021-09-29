IDS=$(xinput | grep Wacom | egrep -o "id=[0-9]+" | cut -d= -f2)
DISP=$(xrandr | sed 1D | grep -v "^ " | dmenu -p "Limit Wacom input to display: " -l 10 | cut -d\  -f1)

if [ `xrandr | grep $DISP 2>/dev/null | wc -l` -eq 1 ]; then
	for i in $IDS; do
		xinput map-to-output $i $DISP
	done
else
	echo "No display selected"
	exit 1
fi
