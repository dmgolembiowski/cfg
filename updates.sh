#!/bin/sh

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/ver.sh

for n in prometheus alertmanager node_exporter; do
	_ver=$(
		curl -s https://api.github.com/repos/prometheus/$n/releases/latest |
			jq .tag_name -j |
			sed 's/^v//'
	)

	eval _cur=\$$(echo $n | tr '[a-z]' '[A-Z]')_V
	if [ "$_cur" != "$_ver" ]; then
		echo $n $_cur '->' $_ver
	fi
done
