#!/bin/sh

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/ver.sh


gh() {
	local u="$1"
	local n="$2"
	local _cur="$3"

	local _latest=$(
		curl -s https://api.github.com/repos/$u/$n/releases/latest |
			jq .tag_name -j |
			sed 's/^v//'
		  )

	if [ -z "$_cur" ]; then
		eval _cur=\$$(echo $n | tr '[a-z]' '[A-Z]')_V
	fi

	if [ "$_cur" != "$_latest" ]; then
		echo $n $_cur '->' $_latest
	fi
}

for n in prometheus alertmanager node_exporter; do
	gh prometheus $n
done
