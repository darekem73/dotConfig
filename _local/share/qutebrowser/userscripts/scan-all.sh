#!/bin/bash
count=0
[ -f /tmp/finished ] && rm /tmp/finished
[ -f /tmp/end ] && rm /tmp/end
(( count=$count+1 ))
echo "starting scan $count"
qutebrowser ':spawn --userscript scan-one'
while true; do
	if [ -f /tmp/finished ]; then
	       	echo "closing tab"
		rm /tmp/finished
		qutebrowser :tab-close
		(( count=$count+1 ))
		echo "starting scan $count"
		qutebrowser ':spawn --userscript scan-one'
	elif [ -f /tmp/end ]; then
		echo "exiting after $count scans"
		rm /tmp/end
		break
	fi
	echo -n "." && sleep 1
done
