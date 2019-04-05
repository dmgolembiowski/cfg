#!/bin/sh

uid=$(loginctl list-users --no-legend | awk '{ print $1 }')

machinectl --uid=$uid shell .host \
	/bin/systemctl start --no-block --user sway-monitor-hotplug
