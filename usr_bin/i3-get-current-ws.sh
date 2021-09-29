wrk=`i3-msg -t get_workspaces | jq -c '.[] | [.num,.visible,.output]' | grep true | sed 's/\[//;s/\]//' | cut -d, -f1`
exit $wrk
