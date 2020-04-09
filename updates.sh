#!/bin/sh

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/ver.sh

_cmp() {
	local t=$1
	local n=$2
	local _latest="$3"
	local _cur="$4"

	if [ -z "$_cur" ]; then
		eval _cur=\$$(echo $n | tr '[a-z]' '[A-Z]' | sed 's/[^A-Z]//g')_V
	fi

	if [ "$_cur" != "$_latest" ]; then
		echo $t $n $_cur '->' $_latest
	fi
}

gh() {
	local u=$1
	local n=$2
	local _cur="$3"

	local _latest=$(
		curl -s https://api.github.com/repos/$u/$n/releases/latest |
			jq .tag_name -j |
			sed 's/^v//'
	)

	_cmp gh $n "$_latest" "$_cur"
}

pypi() {
	local n=$1
	local _cur="$2"

	local _latest=$(
		curl -s https://pypi.org/pypi/$n/json |
			jq .info.version -r
	)

	_cmp py $n "$_latest" "$_cur"
}

for n in prometheus alertmanager node_exporter; do
	gh prometheus $n
done

gh pixelb ps_mem
gh thelounge thelounge

pypi azure-cli
pypi miniflux

if [ -e $ROOT/env/updates.sh ]; then
	. $ROOT/env/updates.sh
fi
