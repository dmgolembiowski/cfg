#!/bin/sh

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/ver.sh

for n in prometheus alertmanager node_exporter; do
	_latest=$(
		curl -s https://api.github.com/repos/prometheus/$n/releases/latest |
			jq .tag_name -j |
			sed 's/^v//'
	)

	eval _cur=\$$(echo $n | tr '[a-z]' '[A-Z]')_V
	if [ "$_cur" != "$_latest" ]; then
		echo $n $_cur '->' $_latest
	fi
done

_kubectl_latest=$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)

if [ "$_kubectl_latest" != "$KUBECTL_V" ]; then
	echo kubectl $KUBECTL_V '->' $_kubectl_latest
fi
